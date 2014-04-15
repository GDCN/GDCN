package se.chalmers.gdcn.network;

import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.NetworkInterface;
import se.chalmers.gdcn.communicationToUI.Operation;
import se.chalmers.gdcn.communicationToUI.OperationFinishedListener;
import se.chalmers.gdcn.files.DeceitfulFileUtils;
import se.chalmers.gdcn.hashcash.Challenge;
import se.chalmers.gdcn.hashcash.Solution;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import se.chalmers.gdcn.replica.ReplicaBox;

import java.io.Serializable;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ExecutorService;

/**
 * Created by Leif on 2014-03-29.
 *
 * Only ONE Passer object may be created for each peer! Handles all messages to and from this Peer.
 */
public class TaskPasserDeny extends Passer {

    private final NetworkInterface client;
    private final WorkerID myWorkerID;

    private final Map<String, byte[]> falseResults;
    private final ExecutorService pool;

    private static final Random random = new Random();

    /**
     * Message passer for sending messages regarding tasks. OBS! Only ONE Passer may be present for a Peer.
     *
     * @param peer This peer
     * @param client Client to put and get results
     * @param falseResults
     * @param pool
     */
    public TaskPasserDeny(Peer peer, NetworkInterface client, Map<String, byte[]> falseResults, ExecutorService pool) {
        super(peer);
        this.falseResults = falseResults;
        this.pool = pool;
        this.myWorkerID = new WorkerID(peer.getPeerBean().getKeyPair().getPublic());
        this.client = client;
    }

    /**
     * Starts task process working for this peer
     *
     * Works concurrently to solve challenges!
     * @param jobOwner Peer to work for
     */
    public void requestWork(final PeerAddress jobOwner){
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
                Runnable solver = new Runnable() {
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

                                        workOnTaskDeceitfully(jobOwner, replicaBox);
                                        System.out.println("Some Task was received from " + Passer.print(jobOwner));
                                        break;
                                    case NO_TASK_AVAILABLE:
                                        System.out.println("No task available form this job owner any more");
                                        //TODO
                                        break;
                                    case CHALLENGE_FAIL:
                                        throw new IllegalStateException("Solution failed: " + taskMessage2.getActualContent());
                                    default:
                                        throw new IllegalStateException("Should be a Challenge response here!");
                                }
                            }
                        });
                    }
                };
                pool.submit(solver);
            }
        });
    }


    /**
     * Works on this task until finished.
     * Sybil method!
     *
     * @param jobOwner Peer to send result to
     * @param replicaBox Task (replica) to work on
     */
    private void workOnTaskDeceitfully(final PeerAddress jobOwner, final ReplicaBox replicaBox){
        final Number160 resultKey = replicaBox.getResultKey();
        final String replicaID = replicaBox.getReplicaID();

        System.out.println("Task " + replicaID + " finished. Attempt to upload and notify job owner.");

        client.addListener(new OperationFinishedListener(client, resultKey, CommandWord.PUT) {
            @Override
            protected void operationFinished(Operation operation) {
                if(operation.isSuccess()){
                    System.out.println("Task "+replicaID+" finished. Job owner notified if still online.");
                    sendNoReplyMessage(jobOwner, new TaskMessage(TaskMessageType.RESULT_UPLOADED, myWorkerID,
                            replicaBox.getReplicaID()));
                } else {
                    //
                }
            }
        });

        byte[] result = null;
        //Uses ConcurrentHashMap so this synchronization is actually not needed?
        synchronized (falseResults)
        {
            final String realTask = DeceitfulFileUtils.deduceTask(replicaBox.getTaskMeta());
            result = falseResults.get(realTask);
            if(result == null){
                result = generateFalseResult();
                falseResults.put(realTask, result);
            }
        }
        client.put(resultKey, new Data(result));
        System.out.println("Answer to "+replicaID+" got "+result.length+" bytes");
    }

    private static byte[] generateFalseResult(){
        byte[] result = new byte[100+random.nextInt(300)];
        random.nextBytes(result);
        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    synchronized protected Serializable handleRequest(PeerAddress sender, Object messageContent) {

        TaskMessage taskMessage = TaskMessage.check(messageContent);
        WorkerID workerID = taskMessage.getSenderID();
        return null;

    }

    /**
     * {@inheritDoc}
     */
    @Override
    synchronized protected void handleNoReply(PeerAddress sender, Object messageContent) {
        TaskMessage taskMessage = TaskMessage.check(messageContent);
    }

}
