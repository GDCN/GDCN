package network;

import challenge.Challenge;
import challenge.Solution;
import control.WorkerNodeManager;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.PeerAddress;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by Leif on 2014-03-29.
 *
 * Only ONE Passer object may be created for each peer! Handles all messages to and from this Peer.
 */
public class TaskPasser extends Passer {

    private final WorkerNodeManager workerNodeManager = new WorkerNodeManager(WorkerNodeManager.DisciplinaryAction.REMOVE);

    public TaskPasser(Peer peer) {
        super(peer);
    }

    //This Map is held by JobOwner to remember the current challenges workers and Sybil nodes are solving
    private final Map<String, Challenge> pendingChallenges = new HashMap<>();

    /**
     * Debug message. Just sends a request that is answered.
     * @param otherPeer peer
     * @param hello String of words
     */
    public void sendHello(PeerAddress otherPeer, String hello){
        sendRequest(otherPeer, new TaskMessage(TaskMessageType.HELLO, hello), new OnReplyCommand() {
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

        sendRequest(jobOwner, new TaskMessage(TaskMessageType.REQUEST_CHALLENGE, ""), new OnReplyCommand() {
            @Override
            public void execute(Object replyMessageContent) {
                TaskMessage taskMessage = check(replyMessageContent);
                if (taskMessage.type != TaskMessageType.CHALLENGE) {
                    throw new IllegalStateException("Should be a Challenge response here!");
                }
                Solution challengeSolution = challengeReceived(taskMessage.actualContent);

                System.out.println("Challenge received and solved");

                sendRequest(jobOwner, new TaskMessage(TaskMessageType.REQUEST_TASK, challengeSolution), new OnReplyCommand() {
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
                                throw new IllegalStateException("Solution failed: "+taskMessage2.actualContent);
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
        sendNoReplyMessage(jobOwner, new TaskMessage(TaskMessageType.RESULT_UPLOADED, taskID));
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
        //TODO set workerID as what we really want
        WorkerNodeManager.WorkerID workerID = new WorkerNodeManager.WorkerID(sender);

        switch(taskMessage.type){
            case REQUEST_CHALLENGE:

                System.out.println("Received request for a Challenge");

                //TODO PeerAddress sender doesn't always refer to the correct Peer!!! Send WorkerID in message instead!

                Challenge challenge = workerNodeManager.isWorkerRegistered(workerID)?
                        Challenge.generate() : Challenge.generateHard();
                pendingChallenges.put(challenge.getKey(), challenge);
                return new TaskMessage(TaskMessageType.CHALLENGE, challenge);

            case REQUEST_TASK:

                System.out.println("Received request for a Task");

                Solution solution = (Solution) taskMessage.actualContent;
                Challenge originalChallenge = pendingChallenges.remove(solution.getKey());

                if(originalChallenge != null && originalChallenge.isSolution(solution)){
                    workerNodeManager.registerWorker(workerID);

                    //TODO give actual task information
                    return new TaskMessage(TaskMessageType.TASK, "TODO An actual serializable TaskMeta here please");

                } else {
                    workerNodeManager.reportWorker(workerID);
                    if(originalChallenge == null){
                        return new TaskMessage(TaskMessageType.FAIL, "Provided solution didn't match any challenge!");
                    }
                    return new TaskMessage(TaskMessageType.FAIL, "Provided solution was FALSE!");
                }

            case HELLO:
                System.out.println("Received Hello: "+taskMessage.actualContent.toString());
                return new TaskMessage(TaskMessageType.HELLO, "Hi, I heard you said "+taskMessage.actualContent);

            default:
                throw new UnsupportedOperationException("Unsupported request: "+taskMessage.type);
        }
    }

    @Override
    synchronized protected void handleNoReply(PeerAddress sender, Object messageContent) {
        TaskMessage taskMessage = check(messageContent);

        switch (taskMessage.type){
            case RESULT_UPLOADED:
                resultUploaded(taskMessage.actualContent);
                break;
            default:
                throw new UnsupportedOperationException("Unsupported request: "+taskMessage.type);
        }
    }

    private void resultUploaded(Object resultCode){
        System.out.println("Apparently some task was completed");
        //TODO download result. Mark that replica as solved.
        //TODO If Task complete: validate...
        //TODO promote workers who did well
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
        private TaskMessageType type;
        private Object actualContent;

        private TaskMessage(TaskMessageType type, Object actualContent) {
            this.type = type;
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
