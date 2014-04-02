package network;

import challenge.Challenge;
import challenge.Solution;
import command.communicationToUI.ClientInterface;
import command.communicationToUI.CommandWord;
import command.communicationToUI.OperationFinishedEvent;
import control.TaskManager;
import control.WorkerNodeManager;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import replica.ReplicaBox;
import replica.ReplicaManager;
import taskbuilder.communicationToClient.TaskListener;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by Leif on 2014-03-29.
 *
 * Only ONE Passer object may be created for each peer! Handles all messages to and from this Peer.
 */
public class TaskPasser extends Passer {

    private final WorkerNodeManager workerNodeManager = new WorkerNodeManager(WorkerNodeManager.DisciplinaryAction.REMOVE, 3);
    private final ReplicaManager replicaManager;
    private final TaskManager taskManager;
    private final ClientInterface client;

    private final WorkerID myWorkerID;

    /**
     * Message passer for sending messages regarding tasks. OBS! Only ONE Passer may be present for a Peer.
     *
     * @param peer This peer
     * @param replicaManager Manager to ask for Replicas that are sent to workers
     * @param taskManager Manager to run a task (replica) that was received
     * @param client
     */
    public TaskPasser(Peer peer, ReplicaManager replicaManager, TaskManager taskManager, ClientInterface client) {
        super(peer);
        this.replicaManager = replicaManager;
        this.taskManager = taskManager;
        this.myWorkerID = new WorkerID(peer.getPeerBean().getKeyPair().getPublic());
        this.client = client;
    }

    //This Map is held by JobOwner to remember the current challenges workers and Sybil nodes are solving
    private final Map<String, Challenge> pendingChallenges = new HashMap<>();

    /**
     * Debug message. Just sends a request that is answered.
     * @param otherPeer peer
     * @param hello String of words
     */
    public void sendHello(PeerAddress otherPeer, String hello){
        sendRequest(otherPeer, new TaskMessage(TaskMessageType.HELLO, myWorkerID, hello), new OnReplyCommand() {
            @Override
            public void execute(Object replyMessageContent) {
                TaskMessage taskMessage = check(replyMessageContent);
                System.out.println(taskMessage.actualContent);
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
        System.out.println("Request work from "+Passer.print(jobOwner));

        sendRequest(jobOwner, new TaskMessage(TaskMessageType.REQUEST_CHALLENGE, myWorkerID, ""), new OnReplyCommand() {
            @Override
            public void execute(Object replyMessageContent) {
                TaskMessage taskMessage = check(replyMessageContent);
                if (taskMessage.type != TaskMessageType.CHALLENGE) {
                    throw new IllegalStateException("Should be a Challenge response here!");
                }
                Solution challengeSolution = challengeReceived(taskMessage.actualContent);

                System.out.println("Challenge received and solved");

                sendRequest(jobOwner, new TaskMessage(TaskMessageType.REQUEST_TASK, myWorkerID, challengeSolution), new OnReplyCommand() {
                    @Override
                    public void execute(Object replyMessageContent2) {
                        TaskMessage taskMessage2 = check(replyMessageContent2);
                        switch (taskMessage2.type) {
                            case TASK:
                                ReplicaBox replicaBox = (ReplicaBox) taskMessage2.actualContent;
                                System.out.println("Start processing task");

                                workOnTask(jobOwner, replicaBox);
                                System.out.println("Some Task was received from " + Passer.print(jobOwner));
                                break;
                            case CHALLENGE_FAIL:
                                throw new IllegalStateException("Solution failed: " + taskMessage2.actualContent);
                            default:
                                throw new IllegalStateException("Should be a Challenge response here!");
                        }
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
        taskManager.startTask("Primes", replicaBox.getTaskMeta(), new TaskListener() {
            @Override
            public void taskFinished(final String taskName) {

                final Number160 resultKey = replicaBox.getResultKey();
                System.out.println("Task "+taskName+" finished. Attempt to upload and notify job owner.");

                client.addListener(new PropertyChangeListener() {
                    @Override
                    public void propertyChange(PropertyChangeEvent evt) {
                        if(!(evt instanceof OperationFinishedEvent)){
                            return;
                        }
                        OperationFinishedEvent event = (OperationFinishedEvent) evt;
                        if( event.getCommandWord() != CommandWord.PUT){
                            return;
                        }
                        if(event.getOperation().getKey().equals(resultKey.toString())){
                            if(event.getOperation().isSuccess()){
                                System.out.println("Task "+taskName+" finished. Job owner notified if still online.");
                                sendNoReplyMessage(jobOwner, new TaskMessage(TaskMessageType.RESULT_UPLOADED, myWorkerID,
                                        replicaBox.getReplicaID()));
                            } else {
                                taskFailed(taskName, "Couldn't upload result to DHT :P");
                            }

                            client.removeListener(this);
                        }
                    }
                });
                //TODO Upload result of task here! not null!
                try {
                    client.put(resultKey, new Data("NullEmptyResult"));
                } catch (IOException e) {
                    e.printStackTrace();
                    taskFailed(taskName, e.getMessage());
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

    private static class FailMessage implements Serializable{
        private final String reason;
        private final String ID;

        private FailMessage(String reason, String ID) {
            this.reason = reason;
            this.ID = ID;
        }
    }

    private Solution challengeReceived(Object challengeData){
        // TODO real challenge, not just mock up challenge
        Challenge challenge = (Challenge) challengeData;
        return Solution.solve(challenge);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    synchronized protected Serializable handleRequest(PeerAddress sender, Object messageContent) {

        TaskMessage taskMessage = check(messageContent);
        WorkerID workerID = taskMessage.senderID;

        switch(taskMessage.type){
            case REQUEST_CHALLENGE:

                System.out.println("Received request for a Challenge");

                Challenge challenge = workerNodeManager.isWorkerRegistered(workerID)?
                        Challenge.generate() : Challenge.generateHard();
                pendingChallenges.put(challenge.getKey(), challenge);
                return new TaskMessage(TaskMessageType.CHALLENGE, myWorkerID, challenge);

            case REQUEST_TASK:

                System.out.println("Received request for a Task");

                Solution solution = (Solution) taskMessage.actualContent;
                Challenge originalChallenge = pendingChallenges.remove(solution.getKey());

                if(originalChallenge != null && originalChallenge.isSolution(solution)){
                    workerNodeManager.registerWorker(workerID);

                    ReplicaBox replicaBox = replicaManager.giveReplicaToWorker(workerID);
                    return new TaskMessage(TaskMessageType.TASK, myWorkerID, replicaBox);

                } else {
                    workerNodeManager.reportWorker(workerID);
                    if(originalChallenge == null){
                        return new TaskMessage(TaskMessageType.CHALLENGE_FAIL, myWorkerID, "Provided solution didn't match any challenge!");
                    }
                    return new TaskMessage(TaskMessageType.CHALLENGE_FAIL, myWorkerID, "Provided solution was FALSE!");
                }

            case HELLO:
                System.out.println("Received Hello: "+taskMessage.actualContent.toString());
                return new TaskMessage(TaskMessageType.HELLO, myWorkerID, "Hi, I heard you said "+taskMessage.actualContent);

            default:
                throw new UnsupportedOperationException("Unsupported request: "+taskMessage.type);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    synchronized protected void handleNoReply(PeerAddress sender, Object messageContent) {
        TaskMessage taskMessage = check(messageContent);

        switch (taskMessage.type){
            case RESULT_UPLOADED:
                resultUploaded((String) taskMessage.actualContent);
                break;
            case TASK_FAIL:
                FailMessage failMessage = (FailMessage) taskMessage.actualContent;
                System.out.println("My task failed! Reason: "+failMessage.reason);
                replicaManager.replicaFailed(failMessage.ID);
                break;
            default:
                throw new UnsupportedOperationException("Unsupported request: "+taskMessage.type);
        }
    }

    private void resultUploaded(String replicaID){
        System.out.println("Apparently some task was completed");
        //TODO download result.
        replicaManager.replicaFinished(replicaID, "TODO Put downloaded result here...");
    }

    private static TaskMessage check(Object messageContent){
        if(!(messageContent instanceof TaskMessage)){
            throw new IllegalStateException("Message from is not a TaskMessage! "+messageContent.toString());
        }

        return (TaskMessage) messageContent;
    }

    /**
     * Specific enum used for this message passing interface
     */
    private static enum TaskMessageType{
        REQUEST_CHALLENGE,
        CHALLENGE,
        REQUEST_TASK,
        TASK_FAIL,
        CHALLENGE_FAIL,
        TASK,
        RESULT_UPLOADED,
        HELLO
    }

    /**
     * Class for encapsulating task messages. It is similar to {@link network.NetworkMessage}
     * but more specific for this purpose. NetworkMessage can be used for any purpose and will in this case
     * contain an object of TaskMessage.
     */
    private static class TaskMessage implements Serializable{
        private final TaskMessageType type;
        private final WorkerID senderID;
        private final Object actualContent;

        private TaskMessage(TaskMessageType type, WorkerID senderID, Object actualContent) {
            this.type = type;
            this.senderID = senderID;
            this.actualContent = actualContent;
        }

        @Override
        public String toString() {
            return "TaskMsg{ " + type +
                    ", " + actualContent +
                    '}';
        }
    }
}
