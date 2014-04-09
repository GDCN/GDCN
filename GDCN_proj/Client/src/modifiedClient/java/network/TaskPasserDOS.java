package network;

import hashcash.Challenge;
import hashcash.HashCash;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.PeerAddress;

import javax.crypto.SecretKey;
import java.io.Serializable;

/**
 * Created by Leif on 2014-03-29.
 *
 * Only ONE Passer object may be created for each peer! Handles all messages to and from this Peer.
 *
 * FAKE Sybil version of TaskPasser used for DOS attack
 */
public class TaskPasserDOS extends Passer {

    //TODO secretKey should probably be stored in a better place (and be stored in a file between runs).
    private SecretKey secretKey = null;
    private HashCash hashCash = null;

    private final WorkerID myWorkerID;

    /**
     * Message passer for sending messages regarding tasks. OBS! Only ONE Passer may be present for a Peer.
     *
     * @param peer This peer
     */
    public TaskPasserDOS(Peer peer) {
        super(peer);
        this.myWorkerID = new WorkerID(peer.getPeerBean().getKeyPair().getPublic());

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
    public void requestChallenge(final PeerAddress jobOwner, final OnReplyCommand onReplyCommand){
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

                Challenge challenge = (Challenge) taskMessage.getActualContent();
                onReplyCommand.execute(challenge);
                //Only asks for challenge, nothing more
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
                return null;

            case REQUEST_TASK:

                System.out.println("Received request for a Task");
                return null;

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

    }

}
