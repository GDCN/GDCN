package se.chalmers.gdcn.network;

import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureDHT;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.RequestP2PConfiguration;
import net.tomp2p.p2p.builder.SendBuilder;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.rpc.ObjectDataReply;

import java.io.Serializable;

/**
 * Created by Leif on 2014-03-19.
 *
 * There must only be ONE Passer for each Peer!
 */
abstract class Passer {

    private final Peer peer;

    private final static RequestP2PConfiguration requestConfiguration = new RequestP2PConfiguration(1, 10, 0, false, true);

    protected Passer(final Peer peer) {
        this.peer = peer;
        peer.setObjectDataReply(new ObjectDataReply() {
            @Override
            public Object reply(PeerAddress sender, Object request) throws Exception {

                if(peer.getPeerAddress().equals(sender)){
                    //Disabled for demo
                    //System.out.println("in Passer: ERROR! sender is myself!!!");
                }

                NetworkMessage message = NetworkMessage.decrypt( request);
                if(message == null){
                    //Error has occured in decrypt
                    System.out.println("Decrypt returned NULL!");
                    return "Decrypt was NULL";
                }
                System.out.println("ObjectDataReply received: " + message.toString());

                switch (message.getType()){
                    case REQUEST:

                        System.out.println(print(peer.getPeerAddress())+" received req from "+print(sender));

                        return handleRequest(sender, message.getObject());
                    case NO_REPLY:
                        handleNoReply(sender, message.getObject());
                        return "Message was Handled in some way...";
                }
                return "Message was read but not Handled! Type: "+message.getType().name();
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

    /**
     * Send a message to this Peer and expect an answer
     *
     * @param receiver peer
     * @param message message
     * @param onReturn what you will do when it answers
     */
    protected void sendRequest(final PeerAddress receiver, Serializable message, final OnReplyCommand onReturn){
        SendBuilder sendBuilder = peer.send(receiver.getID());

        final NetworkMessage networkMessage = new NetworkMessage(message, NetworkMessage.Type.REQUEST);

        FutureDHT futureDHT = sendBuilder.setObject( networkMessage.encrypt() ).setRequestP2PConfiguration(requestConfiguration).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {
            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                if(!future.isSuccess()){
                    System.out.println("Error sending " + networkMessage.toString());
                    System.out.println("WHY: "+future.getFailedReason());
                    return;
                }
                System.out.println("Success sending " + networkMessage.toString() + " to " + print(receiver));
                for(PeerAddress address : future.getRawDirectData2().keySet()){
                    Object answer = future.getRawDirectData2().get(address);
                    onReturn.execute(answer);
                    System.out.println(print(address)+" answered with "+answer);
                }
            }
        });
    }

    /**
     * Send message to a peer without expecting something in reply
     * @param receiver peer
     * @param message message
     */
    protected void sendNoReplyMessage(PeerAddress receiver, Serializable message){
        SendBuilder sendBuilder = peer.send(receiver.getID());

        final NetworkMessage networkMessage = new NetworkMessage(message, NetworkMessage.Type.NO_REPLY);

        FutureDHT futureDHT = sendBuilder.setObject( networkMessage.encrypt() ).setRequestP2PConfiguration(requestConfiguration).start();
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
        return peerAddress.getInetAddress().toString()+":"+peerAddress.portTCP();
    }
}
