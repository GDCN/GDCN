package network;

import hashcash.Challenge;
import hashcash.HashCash;
import hashcash.Solution;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.PeerAddress;

import javax.crypto.KeyGenerator;
import java.io.Serializable;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;

/**
 * Created by Leif on 2014-03-29.
 *
 * Only ONE Passer object may be created for each peer! Handles all messages to and from this Peer.
 */
public class TaskPasser extends Passer {
    public TaskPasser(Peer peer) {
        super(peer);

        try {
            secretKey = KeyGenerator.getInstance("HmacSHA256").generateKey();
            hashCash = new HashCash(secretKey);
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (InvalidKeyException e) {
            e.printStackTrace();
        }
    }

    //TODO Key should probably be stored in a better place, please move it if you know where! It is needed *only* for HashCash, is not the same as the private key and should not be shared with *anyone*.
    private Key secretKey = null;
    private HashCash hashCash = null;

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
        return challenge.solve();
    }

    @Override
    synchronized protected Serializable handleRequest(PeerAddress sender, Object messageContent) {

        TaskMessage taskMessage = check(messageContent);

        switch(taskMessage.type){
            case REQUEST_CHALLENGE:

                System.out.println("Received request for a Challenge");

                //TODO if Worker is not registered, generate HARD challenge instead!
                Challenge challenge = hashCash.generateEasyChallenge("1","JobOwner",sender.getID().toString(),"taskID");
                //TODO Insert real seed values. (The first argument is an optional purpose, which should probably either be the id of a replica or "REGISTRATION")
                return new TaskMessage(TaskMessageType.CHALLENGE, challenge);

            case REQUEST_TASK:

                System.out.println("Received request for a Task");

                Solution solution = (Solution) taskMessage.actualContent;

                try {
                    if(solution.isValid(secretKey)){
                        String purpose = solution.getPurpose();

                        if(purpose.equals("REGISTRATION")) {
                            //TODO register Peer in list of workers if not is there already
                        } else {
                            //TODO give actual task information. The replica we should send probably has the same id as above.
                            return new TaskMessage(TaskMessageType.TASK, "An actual serialized TaskMeta here please");
                        }
                    } else {
                        return new TaskMessage(TaskMessageType.FAIL, "Provided solution was FALSE!");
                    }
                } catch (InvalidKeyException e) {
                    e.printStackTrace();
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
                System.out.println("Apparently some task was completed");
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
