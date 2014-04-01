package network;

import challenge.Challenge;
import challenge.Solution;
import control.WorkerNodeManager;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.PeerAddress;
import replica.ReplicaManager;

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

    private final WorkerID myWorkerID;

    public TaskPasser(Peer peer, ReplicaManager replicaManager) {
        super(peer);
        this.replicaManager = replicaManager;
        this.myWorkerID = new WorkerID(peer.getPeerBean().getKeyPair().getPublic());
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
//                                Object taskMeta = taskMessage2.actualContent;
                                //TODO work on Task...
                                System.out.println("Start processing task");

                                workOnTask(jobOwner, "SomeTaskID");
                                System.out.println("Some Task was received from " + Passer.print(jobOwner));
                                break;
                            case FAIL:
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
     * Notify jobOwner that result has been uploaded for this task.
     * @param jobOwner Peer to work for
     * @param taskID ID of task
     */
    public void notifyJobOwner(PeerAddress jobOwner, String taskID){
        System.out.println("JobOwner was notified, if he was still online");
        sendNoReplyMessage(jobOwner, new TaskMessage(TaskMessageType.RESULT_UPLOADED, myWorkerID, taskID));
    }

    /**
     * Dummy method. Remove with actual work
     */
    private void workOnTask(PeerAddress jobOwner, String taskID){
        //TODO work on Task...
        notifyJobOwner(jobOwner, taskID);
    }

    private Solution challengeReceived(Object challengeData){
        // TODO real challenge, not just mock up challenge
        Challenge challenge = (Challenge) challengeData;
        return Solution.solve(challenge);
    }

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

                    String replicaID = replicaManager.giveReplicaToWorker(workerID);
                    //TODO give actual task information
                    return new TaskMessage(TaskMessageType.TASK, myWorkerID, "TODO An actual serializable TaskMeta here please "+replicaID);

                } else {
                    workerNodeManager.reportWorker(workerID);
                    if(originalChallenge == null){
                        return new TaskMessage(TaskMessageType.FAIL, myWorkerID, "Provided solution didn't match any challenge!");
                    }
                    return new TaskMessage(TaskMessageType.FAIL, myWorkerID, "Provided solution was FALSE!");
                }

            case HELLO:
                System.out.println("Received Hello: "+taskMessage.actualContent.toString());
                return new TaskMessage(TaskMessageType.HELLO, myWorkerID, "Hi, I heard you said "+taskMessage.actualContent);

            default:
                throw new UnsupportedOperationException("Unsupported request: "+taskMessage.type);
        }
    }

    @Override
    synchronized protected void handleNoReply(PeerAddress sender, Object messageContent) {
        TaskMessage taskMessage = check(messageContent);

        switch (taskMessage.type){
            case RESULT_UPLOADED:
                resultUploaded((String) taskMessage.actualContent);
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

    private static enum TaskMessageType{
        REQUEST_CHALLENGE,
        CHALLENGE,
        REQUEST_TASK,
        FAIL,
        TASK,
        RESULT_UPLOADED,
        HELLO
    }

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
            return "TaskM{ " + type +
                    ", " + actualContent +
                    '}';
        }
    }
}
