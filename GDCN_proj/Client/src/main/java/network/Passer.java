package network;

import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureDHT;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.RequestP2PConfiguration;
import net.tomp2p.p2p.builder.SendBuilder;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.rpc.ObjectDataReply;

import javax.crypto.SecretKey;
import javax.crypto.interfaces.DHPublicKey;
import java.io.Serializable;
import java.security.InvalidKeyException;
import java.security.KeyPair;
import java.security.PrivateKey;
import java.security.PublicKey;
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

    protected final Map<PeerAddress,PeerKeys> knownKeys = new HashMap<>();

    //TODO save dhKeys.
    private final KeyPair dhKeys = Crypto.generateAgreementKeyPair();

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
                    PublicKey exchangeKey = handshake.agreementKey;

                    SecretKey secretKey = Crypto.generateSecretKey(dhKeys.getPrivate(), exchangeKey);
                    PeerKeys peerKeys = new PeerKeys<>(handshake.signKey, secretKey);

                    knownKeys.put(sender, peerKeys);

                    return handshake.reply(dhKeys.getPublic(), getPublicKey());
                } else if (request instanceof byte[]) {
                    SecretKey secretKey = knownKeys.get(sender).secretKey;

                    if(secretKey == null) {
                        System.out.println("Sender has not shaken hands, cannot decrypt! "+sender);
                        return "Sender has not shaken hands, cannot decrypt! "+sender;
                    }

                    NetworkMessage message = NetworkMessage.decrypt((byte[]) request, secretKey);

                    if(message == null){
                        //Error has occured in decrypt
                        System.out.println("Decrypt returned NULL!");
                        return "Decrypt was NULL";
                    }
                    System.out.println("ObjectDataReply received: " + message.toString());

                    switch (message.getType()){
                        case REQUEST:
                            return handleRequest(sender, message.getObject());
                        case NO_REPLY:
                            handleNoReply(sender, message.getObject());
                            return "Message was Handled in some way...";
                    }
                    return "Message was read but not Handled! Type: "+message.getType().name();
                }
                return "The request was neither a Handshake nor encrypted. Nothing to be done. Request: "+request;
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

    //TODO make either private or protected final. If chose protected -> add javadoc
    protected void sendHandshake(final PeerAddress receiver, final OnReplyCommand onCompletion) {
        final Handshake handshake = new Handshake((DHPublicKey) dhKeys.getPublic(), getPublicKey());
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

                            if (handshakeReply.phase == Handshake.Phase.REPLY) {
                                PublicKey receiverKey = handshakeReply.agreementKey;
                                SecretKey secretKey;
                                try {
                                    secretKey = Crypto.generateSecretKey(dhKeys.getPrivate(), receiverKey);
                                } catch (InvalidKeyException e) {
                                    e.printStackTrace();
                                    System.out.println("Could not create secret key for node "+receiver+" The agreement keys was invalid.");
                                    return;
                                }

                                PeerKeys peerKeys = new PeerKeys<>(handshake.signKey,secretKey);

                                knownKeys.put(receiver, peerKeys);
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
     *
     * todo make final?
     */
    protected void sendRequest(final PeerAddress receiver, final Serializable message, final OnReplyCommand onReturn) {
        PeerKeys peerKeys = knownKeys.get(receiver);

        if (peerKeys == null) {
            System.out.println("receiver has not shaken hands, cannot encrypt.\nInitiating handshake...");

            sendHandshake(receiver, new OnReplyCommand() {
                @Override
                public void execute(Object replyMessageContent) {
                    sendRequest(receiver,message,onReturn);
                }
            });
            return;
        }

        SecretKey sharedKey = peerKeys.secretKey;

        Serializable readyMessage;
        SendBuilder sendBuilder = peer.send(receiver.getID());

        NetworkMessage networkMessage = new NetworkMessage(message, NetworkMessage.Type.REQUEST);


        try {
            readyMessage = networkMessage.encrypt(sharedKey);
        } catch (InvalidKeyException e) {
            e.printStackTrace();
            System.out.println("in Passer: Invalid key when encrypting message. Message: "+networkMessage);
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
     * todo make final?
     */
    protected void sendNoReplyMessage(final PeerAddress receiver, final Serializable message) {
        SendBuilder sendBuilder = peer.send(receiver.getID());

        final NetworkMessage networkMessage = new NetworkMessage(message, NetworkMessage.Type.NO_REPLY);

        PeerKeys peerKeys = knownKeys.get(receiver);

        if(peerKeys == null) {
            System.out.println("receiver has not shaken hands, cannot encrypt.\nInitiating handshake...");

            sendHandshake(receiver, new OnReplyCommand() {
                @Override
                public void execute(Object replyMessageContent) {
                    sendNoReplyMessage(receiver, message);
                }
            });
            return;
        }

        SecretKey sharedKey = peerKeys.secretKey;

        byte[] encryptedMessage;

        try {
            encryptedMessage = networkMessage.encrypt(sharedKey);
        } catch (InvalidKeyException e) {
            e.printStackTrace();
            System.out.println("in Passer: Failure during encryption, invalid key! Message: "+networkMessage);
            return;
        }

        FutureDHT futureDHT = sendBuilder.setObject(encryptedMessage).setRequestP2PConfiguration(requestConfiguration).start();

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

    /**
     * Gets this peer's private key.
     * @return The private key.
     */
    final protected PrivateKey getPrivateKey() {
        return peer.getPeerBean().getKeyPair().getPrivate();
    }

    /**
     * Gets this peer's public key.
     * @return The public key.
     */
    final protected PublicKey getPublicKey() {
        return peer.getPeerBean().getKeyPair().getPublic();
    }

    protected class PeerKeys<P extends PublicKey> {
        public final P publicKey;
        public final SecretKey secretKey;

        public PeerKeys(P publicKey, SecretKey secretKey) {
            this.publicKey = publicKey;
            this.secretKey = secretKey;
        }
    }
}
