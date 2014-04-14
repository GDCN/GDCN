package se.chalmers.gdcn.network;

import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.NetworkInterface;
import se.chalmers.gdcn.communicationToUI.Operation;
import se.chalmers.gdcn.communicationToUI.OperationFinishedListener;
import se.chalmers.gdcn.control.TaskManager;
import se.chalmers.gdcn.control.WorkerNodeManager;
import se.chalmers.gdcn.files.DataFilesManager;
import se.chalmers.gdcn.files.FileUtils;
import se.chalmers.gdcn.hashcash.Challenge;
import se.chalmers.gdcn.hashcash.HashCash;
import se.chalmers.gdcn.hashcash.Solution;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import se.chalmers.gdcn.replica.ReplicaBox;
import se.chalmers.gdcn.replica.ReplicaManager;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Created by Leif on 2014-03-29.
 *
 * Only ONE Passer object may be created for each peer! Handles all messages to and from this Peer.
 */
public class TaskPasser extends Passer {

    private final WorkerNodeManager workerNodeManager;
    private final ReplicaManager replicaManager;
    private final TaskManager taskManager;
    private final NetworkInterface client;

    private DataFilesManager dataFilesManager;
    private Timer timer;

    //TODO secretKey should probably be stored in a better place (and be stored in a file between runs).
    private SecretKey secretKey = null;
    private HashCash hashCash = null;

    private final WorkerID myWorkerID;

    /**
     * Message passer for sending messages regarding tasks. OBS! Only ONE Passer may be present for a Peer.
     *
     * @param peer This peer
     * @param replicaManager Manager to ask for Replicas that are sent to workers
     * @param taskManager Manager to run a task (replica) that was received
     * @param client Client to put and get results
     */
    public TaskPasser(Peer peer, final ReplicaManager replicaManager, TaskManager taskManager, NetworkInterface client, DataFilesManager dm) {
        super(peer);
        this.replicaManager = replicaManager;
        this.taskManager = taskManager;
        this.myWorkerID = new WorkerID(peer.getPeerBean().getKeyPair().getPublic());
        this.client = client;
        this.dataFilesManager = dm;

        secretKey = dataFilesManager.getSecretKey();

        if(secretKey == null) {
            try {
                secretKey = KeyGenerator.getInstance("HmacSHA256").generateKey();
                dataFilesManager.saveSecretKey(secretKey);
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
            }
        }
        try {
            hashCash = new HashCash(secretKey);
        } catch (InvalidKeyException e) {
            e.printStackTrace();
        }

        WorkerNodeManager workerNodeManager1 = dataFilesManager.getWorkerNodeManager();

        if (workerNodeManager1 == null) {
            workerNodeManager = new WorkerNodeManager(WorkerNodeManager.DisciplinaryAction.REMOVE, 3);
        } else {
            workerNodeManager = workerNodeManager1;
        }

        timer = new Timer(true);

        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                dataFilesManager.saveWorkerNodeManager(workerNodeManager);

                System.out.println("Saving workerNodeManager");

            }
        }, 1000 * 120, 1000 * 120);


    }

    public void stopTimer() {
        timer.cancel();
    }


    /**
     * Debug message. Just sends a request that is answered.
     * @param otherPeer peer
     * @param hello String of words
     */
    public void sendHello(PeerAddress otherPeer, String hello){
        sendRequest(otherPeer, new TaskMessage(TaskMessageType.HELLO, myWorkerID, hello), new OnReplyCommand() {
            @Override
            public void execute(Object replyMessageContent) {
                TaskMessage taskMessage = TaskMessage.check(replyMessageContent);
                System.out.println(taskMessage.getActualContent());
            }
        });
    }

    /**
     * Starts task process working for this peer
     * @param jobOwner Peer to work for
     */
    public void requestWork(final PeerAddress jobOwner){
        //TODO do concurrently?
        //TODO make tread safe...
        System.out.println("Request work from " + Passer.print(jobOwner));

        sendRequest(jobOwner, new TaskMessage(TaskMessageType.REQUEST_CHALLENGE, myWorkerID, ""), new OnReplyCommand() {
            @Override
            public void execute(Object replyMessageContent) {
                TaskMessage taskMessage = TaskMessage.check(replyMessageContent);
                if (taskMessage.getType() != TaskMessageType.CHALLENGE) {
                    throw new IllegalStateException("Should be a Challenge response here!");
                }

                final Challenge challenge = (Challenge) taskMessage.getActualContent();
                System.out.println("Challenge received: "+challenge.toString());

                taskManager.submit(new Runnable() {
                    @Override
                    public void run() {
                        Solution challengeSolution = challenge.solve();
                        System.out.println("Challenge solved");

                        sendRequest(jobOwner, new TaskMessage(TaskMessageType.REQUEST_TASK, myWorkerID, challengeSolution), new OnReplyCommand() {
                            @Override
                            public void execute(Object replyMessageContent2) {
                                TaskMessage taskMessage2 = TaskMessage.check(replyMessageContent2);
                                switch (taskMessage2.getType()) {
                                    case TASK:
                                        ReplicaBox replicaBox = (ReplicaBox) taskMessage2.getActualContent();
                                        System.out.println("Start processing task, \n\tResultKey: "+replicaBox.getResultKey());

                                        workOnTask(jobOwner, replicaBox);
                                        System.out.println("Some Task was received from " + Passer.print(jobOwner));
                                        break;
                                    case CHALLENGE_FAIL:
                                        throw new IllegalStateException("Solution failed: " + taskMessage2.getActualContent());
                                    default:
                                        throw new IllegalStateException("Should be a Challenge response here!");
                                }
                            }
                        });
                    }
                });
            }
        });
    }

    /**
     * Works on this task until finished. Calls job owner when done or when failed.
     * @param jobOwner Peer to send result to
     * @param replicaBox Task (replica) to work on
     */
    private void workOnTask(final PeerAddress jobOwner, final ReplicaBox replicaBox){
        //TODO project name?
        final StringHolder stringHolder = new StringHolder();

        taskManager.startTask("Primes", replicaBox.getTaskMeta(), stringHolder, new TaskListener() {
            @Override
            public void taskFinished(final String taskName) {

                final Number160 resultKey = replicaBox.getResultKey();
                System.out.println("Task " + taskName + " finished. Attempt to upload and notify job owner.");

                client.addListener(new OperationFinishedListener(client, resultKey, CommandWord.PUT) {
                    @Override
                    protected void operationFinished(Operation operation) {
                        if(operation.isSuccess()){
                            System.out.println("Task "+taskName+" finished. Job owner notified if still online.");
                            sendNoReplyMessage(jobOwner, new TaskMessage(TaskMessageType.RESULT_UPLOADED, myWorkerID,
                                    replicaBox.getReplicaID()));
                        } else {
                            taskFailed(taskName, "Couldn't upload result to DHT :P");
                        }
                    }
                });
                //TODO sign result with private key... Might want to use the class 'Box' or similar for signing
                byte[] result = null;
                try {
                    result = FileUtils.fromFile(new File(stringHolder.getString()));
                } catch (IOException e) {
                    e.printStackTrace();
                    taskFailed(taskName, e.getMessage());
                }
                if(result != null){
                    System.out.println("\nResult holds "+result.length+" bytes.");
                    client.put(resultKey, new Data(result));
                }
            }

            @Override
            public void taskFailed(String taskName, String reason) {
                System.out.println("Task "+taskName+" failed. Job owner notified if still online. Reason: "+reason);
                sendNoReplyMessage(jobOwner, new TaskMessage(TaskMessageType.TASK_FAIL, myWorkerID,
                        new FailMessage(reason, replicaBox.getReplicaID())));
            }
        });

    }

    /**
     * {@inheritDoc}
     */
    @Override
    synchronized protected Serializable handleRequest(PeerAddress sender, Object messageContent) {

        TaskMessage taskMessage = TaskMessage.check(messageContent);
        WorkerID workerID = taskMessage.getSenderID();

        switch(taskMessage.getType()){
            case REQUEST_CHALLENGE:

                System.out.println("Received request for a Challenge");

                Challenge challenge = workerNodeManager.isWorkerRegistered(workerID)?
                        hashCash.generateAuthenticationChallenge(myWorkerID, workerID)
//                        : hashCash.generateChallenge(HashCash.Purpose.REGISTER, myWorkerID.toString() + workerID.toString(), 15);
                        : hashCash.generateRegistrationChallenge(myWorkerID, workerID);
                return new TaskMessage(TaskMessageType.CHALLENGE, myWorkerID, challenge);

            case REQUEST_TASK:

                System.out.println("Received request for a Task");
                Solution solution = (Solution) taskMessage.getActualContent();

                try {
                    if(solution.isValid(secretKey)) {
                        if(solution.getPurpose() == HashCash.Purpose.REGISTER) {
                            workerNodeManager.registerWorker(workerID);
                        }
                        ReplicaBox replicaBox = replicaManager.giveReplicaToWorker(workerID);
                        if(replicaBox==null){
                            return new TaskMessage(TaskMessageType.NO_TASK_AVAILABLE, myWorkerID, "");
                        }
                        System.out.println("Gave replica "+replicaBox.getReplicaID()+"\n\tResultKey: "+replicaBox.getResultKey());
                        return new TaskMessage(TaskMessageType.TASK, myWorkerID, replicaBox);

                    } else {
                        workerNodeManager.reportWorker(workerID);
                        return new TaskMessage(TaskMessageType.CHALLENGE_FAIL, myWorkerID, "Provided solution was FALSE!");
                    }
                } catch (InvalidKeyException e) {
                    e.printStackTrace();
                }

            case HELLO:
                System.out.println("Received Hello: "+taskMessage.getActualContent().toString());
                return new TaskMessage(TaskMessageType.HELLO, myWorkerID, "Hi, I heard you said "+taskMessage.getActualContent());

            default:
                throw new UnsupportedOperationException("Unsupported request: "+taskMessage.getType());
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    synchronized protected void handleNoReply(PeerAddress sender, Object messageContent) {
        TaskMessage taskMessage = TaskMessage.check(messageContent);

        switch (taskMessage.getType()){
            case RESULT_UPLOADED:
                resultUploaded((String) taskMessage.getActualContent());
                break;
            case TASK_FAIL:
                FailMessage failMessage = (FailMessage) taskMessage.getActualContent();
                WorkerID worker = taskMessage.getSenderID();
                //TODO check reputation as well?
                if(replicaManager.isWorkerAssignedReplica(worker, failMessage.getReplicaID())){
                    System.out.println("My task failed! Reason: "+failMessage.getReason());
                    replicaManager.replicaFailed(failMessage.getReplicaID());
                } else {
                    System.out.println("Warning! A worker node reported a failure in a task it was not participating in...");
                    workerNodeManager.reportWorker(worker);
                }
                break;
            default:
                throw new UnsupportedOperationException("Unsupported request: "+taskMessage.getType());
        }
    }

    /**
     * Called when the job owner has been notified that a certain result has been uploaded.
     * @param replicaID ID of the replica who's result was uploaded
     */
    private void resultUploaded(final String replicaID){
        System.out.println("Replica was completed: "+replicaID);

        final Number160 resultKey = replicaManager.getReplicaResultKey(replicaID);
//        System.out.println("\tResultKey: "+resultKey);

        client.addListener(new OperationFinishedListener(client, resultKey, CommandWord.GET) {
            @Override
            protected void operationFinished(Operation operation) {
                if (operation.isSuccess()) {
//                    System.out.println("RESULT RAW: "+operation.getResult().toString());
                    Data resultData = (Data) operation.getResult();

                    byte[] resultArray = resultData.getData();
                    System.out.println("Result downloaded successfully, \n\tresult holds "+resultArray.length+" bytes.");
                    replicaManager.replicaFinished(replicaID, resultArray);
                } else {
                    System.out.println("DownloadOperation failed! " + operation.getErrorCode()
                            + "\n\t" + operation.getReason());
                }
            }
        });
        client.get(resultKey);

    }

}
