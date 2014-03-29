package network;

import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.PeerAddress;

/**
 * Created by Leif on 2014-03-29.
 *
 * Only ONE Passer object may be created for each peer! Handles all messages to and from this Peer.
 */
public class TaskPasser extends Passer {
    public TaskPasser(Peer peer) {
        super(peer);
    }

    /**
     * Starts task process working for this peer
     * @param jobOwner Peer to work for
     */
    public void requestWork(final PeerAddress jobOwner){
        sendRequest(jobOwner, new TaskMessage(TaskMessageType.REQUEST_CHALLENGE, null), new OnReplyCommand() {
            @Override
            public void execute(Object replyMessageContent) {
                TaskMessage taskMessage = check(replyMessageContent);
                if (taskMessage.type != TaskMessageType.CHALLENGE) {
                    throw new IllegalStateException("Should be a Challenge response here!");
                }
                Object challengeSolution = challengeReceived(taskMessage.actualContent);

                //TODO send solution

                sendRequest(jobOwner, new TaskMessage(TaskMessageType.REQEST_TASK, null), new OnReplyCommand() {
                    @Override
                    public void execute(Object replyMessageContent) {

                    }
                });
            }
        });
    }

    private Object challengeReceived(Object challengeData){
        //TODO solve challenge
        return "Some solution";
    }

    @Override
    protected Object handleRequest(PeerAddress sender, Object messageContent) {

        TaskMessage taskMessage = check(messageContent);

        switch(taskMessage.type){
            case REQUEST_CHALLENGE:
                //TODO give challenge data
                return new TaskMessage(TaskMessageType.CHALLENGE, "Some challenge data");
            case REQEST_TASK:
                //TODO give actual task information
                return new TaskMessage(TaskMessageType.TASK, "An actual serialized TaskMeta here please");
            case HELLO:
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
        REQEST_TASK,
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
