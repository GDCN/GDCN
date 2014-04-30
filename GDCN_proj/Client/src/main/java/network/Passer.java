package network;

import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureDHT;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.RequestP2PConfiguration;
import net.tomp2p.p2p.builder.SendBuilder;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.rpc.ObjectDataReply;
import javax.crypto.SealedObject;
import javax.crypto.SecretKey;
import javax.crypto.interfaces.DHPublicKey;
import java.io.IOException;
import java.io.Serializable;
import java.security.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by Leif on 2014-03-19.
 *
 * There must only be ONE Passer for each Peer!
 */
abstract class Passer {

    private final Peer peer;

    private final static RequestP2PConfiguration requestConfiguration = new RequestP2PConfiguration(1, 10, 0);

    private final Map<PeerAddress,SecretKey> peerKeys = new HashMap<>();

    private final KeyPair dhKeys = Crypto.generateDHKeyPair();

    public Passer(final Peer peer) {
        this.peer = peer;
        peer.setObjectDataReply(new ObjectDataReply() {
            @Override
            public Object reply(PeerAddress sender, Object request) throws Exception {

                if(peer.getPeerAddress().equals(sender)){
                    System.out.println("in Passer: ERROR! sender is myself!!!");
                }

                if (request instanceof Handshake) {
                    Handshake handshake = (Handshake) request;
                    DHPublicKey senderKey = handshake.dhKey;

                    SecretKey secretKey = Crypto.generateSecretKey(dhKeys.getPrivate(), senderKey);
                    peerKeys.put(sender,secretKey);

                    return handshake.reply((DHPublicKey) dhKeys.getPublic());
                } else if (request instanceof SealedObject) {
                    SecretKey secretKey = peerKeys.get(sender);

                    if(secretKey == null) {
                        System.out.println("Sender has not shaken hands, cannot decrypt! "+sender);
                        return "Sender has not shaken hands, cannot decrypt! "+sender;
                    }

                    NetworkMessage message = NetworkMessage.decrypt((SealedObject) request, secretKey);

                    if(message == null){
                        //Error has occured in decrypt
                        System.out.println("Decrypt returned NULL!");
                        return "Decrypt was NULL";
                    }
                    System.out.println("ObjectDataReply received: " + message.toString());

                    switch (message.getType()){
                        case REQUEST:
                            Serializable reply = handleRequest(sender, message.getObject());
                            return reply;
                        case NO_REPLY:
                            handleNoReply(sender, message.getObject());
                            return "Message was Handled in some way...";
                    }
                    return "Message was read but not Handled! Type: "+message.getType().name();
                }
                return "The request was neither a Handshake nor encrypted. Nothing to be done.";
            }
        });
    }

    /**
     * Handle a Request call from sender
     * @param sender Peer sending
     * @param messageContent Message
     * @return Serializable answer message
     */
    protected abstract Serializable handleRequest(PeerAddress sender, Object messageContent);

    /**
     * Handle a NoReply call from sender
     * @param sender Peer sending
     * @param messageContent Message
     */
    protected abstract void handleNoReply(PeerAddress sender, Object messageContent);

    protected void sendHandshake(final PeerAddress receiver, final OnReplyCommand onCompletion) {
        final Handshake handshake = new Handshake((DHPublicKey) dhKeys.getPublic());
        SendBuilder sendBuilder = peer.send(receiver.getID());

        FutureDHT futureDHT = sendBuilder.setObject(handshake).setRequestP2PConfiguration(requestConfiguration).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {
            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                OnReplyCommand onReturn = new OnReplyCommand() {
                    @Override
                    public void execute(Object replyMessageContent) {
                        if (replyMessageContent instanceof Handshake) {
                            Handshake handshakeReply = (Handshake) replyMessageContent;

                            if (handshakeReply.stage == Handshake.Stage.REPLY) {
                                DHPublicKey receiverKey = handshakeReply.dhKey;
                                SecretKey secretKey = Crypto.generateSecretKey(dhKeys.getPrivate(), receiverKey);

                                peerKeys.put(receiver,secretKey);
                                onCompletion.execute(null); //There is no reply value, and it should never be used in execute()
                            } else {
                                throw new IllegalStateException("Expected a Handshake reply, but got initial Handshake");
                            }
                        } else {
                            throw new IllegalStateException("Should be a Handshake here!");
                        }
                    }
                };

                if(!future.isSuccess()){
                    System.out.println("Error sending " + handshake.toString());
                    System.out.println("WHY: "+future.getFailedReason());
                    return;
                }
                System.out.println("Success sending " + handshake.toString());
                for(PeerAddress address : future.getRawDirectData2().keySet()){
                    Object answer = future.getRawDirectData2().get(address);
                    onReturn.execute(answer);
                    System.out.println(""+print(address)+" answered with "+answer);
                }
            }
        });
    }

    /**
     * Send a message to this Peer and expect an answer
     *
     * @param receiver peer
     * @param message message
     * @param onReturn what you will do when it answers
     */
    protected void sendRequest(final PeerAddress receiver, final Serializable message, final OnReplyCommand onReturn) {
        SecretKey receiverKey = peerKeys.get(receiver);

        if (receiverKey == null) {
            System.out.println("receiver has not shaken hands, cannot encrypt.\nInitiating handshake...");

            sendHandshake(receiver, new OnReplyCommand() {
                @Override
                public void execute(Object replyMessageContent) {
                    sendRequest(receiver,message,onReturn);
                }
            });
        }

        Serializable readyMessage;
        SendBuilder sendBuilder = peer.send(receiver.getID());

        NetworkMessage networkMessage = new NetworkMessage(message, NetworkMessage.Type.REQUEST);


        try {
            readyMessage = networkMessage.encrypt(receiverKey);
        } catch (InvalidKeyException|IOException|SignatureException e) {
            e.printStackTrace();
            System.out.println("in Passer: Error when encrypting and signing message. Message: "+networkMessage);
            return;
        }

        final Serializable finalMessage = readyMessage;

        FutureDHT futureDHT = sendBuilder.setObject(readyMessage).setRequestP2PConfiguration(requestConfiguration).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {
            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                if(!future.isSuccess()){
                    System.out.println("Error sending " + finalMessage.toString());
                    System.out.println("WHY: "+future.getFailedReason());
                    return;
                }
                System.out.println("Success sending " + finalMessage.toString());
                for(PeerAddress address : future.getRawDirectData2().keySet()){
                    Object answer = future.getRawDirectData2().get(address);
                    onReturn.execute(answer);
                    System.out.println(""+print(address)+" answered with "+answer);
                }
            }
        });
    }

    /**
     * Send messaage to a peer without expecting something in reply
     * @param receiver peer
     * @param message message
     */
    protected void sendNoReplyMessage(final PeerAddress receiver, final Serializable message) {
        SendBuilder sendBuilder = peer.send(receiver.getID());

        final NetworkMessage networkMessage = new NetworkMessage(message, NetworkMessage.Type.NO_REPLY);

        SecretKey receiverKey = peerKeys.get(receiver);

        if(receiverKey == null) {
            System.out.println("receiver has not shaken hands, cannot encrypt.\nInitiating handshake...");

            sendHandshake(receiver, new OnReplyCommand() {
                @Override
                public void execute(Object replyMessageContent) {
                    sendNoReplyMessage(receiver, message);
                }
            });
        }

        FutureDHT futureDHT = null;
        try {
            //TODO Get PublicKey from receiver...
            futureDHT = sendBuilder.setObject( networkMessage.encrypt(receiverKey) ).setRequestP2PConfiguration(requestConfiguration).start();
        } catch (InvalidKeyException|SignatureException|IOException e) {
            e.printStackTrace();
            System.out.println("in Passer: Failure during encryption!");
        }

        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {
            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                if(!future.isSuccess()){
                    System.out.println("Error sending " + networkMessage.toString());
                    System.out.println("WHY: "+future.getFailedReason());
                    return;
                }
                System.out.println("Success sending " + networkMessage.toString());
            }
        });
    }

    /**
     *
     * @param peerAddress address to some peer
     * @return Readable string
     */
    public static String print(PeerAddress peerAddress){
        return peerAddress.getInetAddress().toString();
    }


    protected PrivateKey getPrivateKey() {
        return peer.getPeerBean().getKeyPair().getPrivate();
    }

    protected PublicKey getPublicKey() {
        return peer.getPeerBean().getKeyPair().getPublic();
    }
}
