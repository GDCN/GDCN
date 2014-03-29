package network;

import challenge.Challenge;
import challenge.Solution;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.PeerAddress;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by Leif on 2014-03-29.
 *
 * Only ONE Passer object may be created for each peer! Handles all messages to and from this Peer.
 */
public class TaskPasser extends Passer {
    public TaskPasser(Peer peer) {
        super(peer);
    }

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
                System.out.println(replyMessageContent.toString());
            }
        });
    }

    /**
     * Starts task process working for this peer
     * @param jobOwner Peer to work for
     */
    public void requestWork(final PeerAddress jobOwner){
        //TODO do concurrently?
        sendRequest(jobOwner, new TaskMessage(TaskMessageType.REQUEST_CHALLENGE, null), new OnReplyCommand() {
            @Override
            public void execute(Object replyMessageContent) {
                TaskMessage taskMessage = check(replyMessageContent);
                if (taskMessage.type != TaskMessageType.CHALLENGE) {
                    throw new IllegalStateException("Should be a Challenge response here!");
                }
                Solution challengeSolution = challengeReceived(taskMessage.actualContent);

                sendRequest(jobOwner, new TaskMessage(TaskMessageType.REQUEST_TASK, challengeSolution), new OnReplyCommand() {
                    @Override
                    public void execute(Object replyMessageContent2) {
                        TaskMessage taskMessage2 = check(replyMessageContent2);
                        switch (taskMessage2.type){
                            case TASK:
                                Object taskMeta = taskMessage2.actualContent;
                                //TODO work on Task...

                            case FAIL:
                                throw new IllegalStateException("Solution failed!");
                            default:
                                throw new IllegalStateException("Should be a Challenge response here!");
                        }
                    }
                });
            }
        });
    }

    private Solution challengeReceived(Object challengeData){
        // TODO real challenge, not just mock up challenge
        Challenge challenge = (Challenge) challengeData;
        return Solution.solve(challenge);
    }

    @Override
    protected Object handleRequest(PeerAddress sender, Object messageContent) {

        TaskMessage taskMessage = check(messageContent);

        switch(taskMessage.type){
            case REQUEST_CHALLENGE:
                //TODO if Worker is not registered, generate HARD challenge instead!
                Challenge challenge = Challenge.generate();
                pendingChallenges.put(challenge.getKey(), challenge);
                return new TaskMessage(TaskMessageType.CHALLENGE, challenge);

            case REQUEST_TASK:
                Solution solution = (Solution) taskMessage.actualContent;
                Challenge originalChallenge = pendingChallenges.remove(solution.getKey());
                if(originalChallenge != null && originalChallenge.isSolution(solution)){
                    //TODO register Peer in list of workers if not is there already
                    //TODO give actual task information
                    return new TaskMessage(TaskMessageType.TASK, "An actual serialized TaskMeta here please");
                } else {
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
    protected void handleNoReply(PeerAddress sender, Object messageContent) {
        TaskMessage taskMessage = check(messageContent);

        switch (taskMessage.type){
            case RESULT_UPLOADED:
                //TODO download result. Mark that task as solved. After that, validate...
                break;
            default:
                throw new UnsupportedOperationException("Unsupported request: "+taskMessage.type);
        }
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

    private static class TaskMessage{
        private TaskMessageType type;
        private Object actualContent;

        private TaskMessage(TaskMessageType type, Object actualContent) {
            this.type = type;
            this.actualContent = actualContent;
        }
    }
}
