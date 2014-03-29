package network;

import net.tomp2p.futures.BaseFuture;
import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureDHT;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.RequestP2PConfiguration;
import net.tomp2p.p2p.builder.SendBuilder;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.rpc.ObjectDataReply;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Random;

/**
 * Created by Leif on 2014-03-19.
 *
 * There must only be ONE Passer for each Peer!
 */
abstract class Passer {

    private final Peer peer;

    private final Random random = new Random();
    private final HashMap<Long, OnReplyCommand> pendingRequests = new HashMap<>();

    public Passer(final Peer peer) {
        this.peer = peer;
        peer.setObjectDataReply(new ObjectDataReply() {
            @Override
            public Object reply(PeerAddress sender, Object request) throws Exception {

                if(peer.getPeerAddress().equals(sender)){
                    System.out.println("in Passer: ERROR! sender is myself!!!");
                }

                NetworkMessage message = NetworkMessage.decrypt( request);
                if(message == null){
                    //Error has occured in decrypt
                    System.out.println("Decrypt returned NULL!");
                    return null;
                }
                System.out.println("ObjectDataReply: " + message.getType().name());

                switch (message.getType()){
                    case OK:
                        OnReplyCommand resolved = pendingRequests.remove(message.getRef());
                        if(resolved==null){
                            System.out.println("OK received for unknown! Ref "+message.getRef());
                        }else{
                            resolved.execute(message.getObject());
//                            System.out.println("OK received for "+resolved.toString());
                        }
                        break;
                    case REQUEST:
                        //TODO remove these outputs
//                        System.out.println("REQUEST received: "+message.getObject());
                        Serializable reply = handleRequest(sender, message.getObject());
                        sendMessage(sender, new NetworkMessage(reply, NetworkMessage.Type.OK, message.getRef()));
                        break;
                    case NO_REPLY:
//                        System.out.println("NO_REPLY received: "+message.getObject());
                        handleNoReply(sender, message.getObject());
                        break;
                }
                return null;
            }
        });
    }

    protected abstract Serializable handleRequest(PeerAddress sender, Object messageContent);

    protected abstract void handleNoReply(PeerAddress sender, Object messageContent);

    protected void sendRequest(PeerAddress receiver, Serializable data, OnReplyCommand onReturn){
        Long ref = random.nextLong();
        pendingRequests.put(ref, onReturn);
        sendMessage(receiver, new NetworkMessage(data, NetworkMessage.Type.REQUEST, ref));
    }

    protected void sendNoReplyMessage(PeerAddress receiver, Serializable data){
        sendMessage(receiver, new NetworkMessage(data, NetworkMessage.Type.NO_REPLY, random.nextLong()));
    }

    /**
     * In testing, the message gets through but the Future says not successful...
     * Perhaps has something to do with the reply...
     * //TODO check further?
     *
     * @param receiver other peer
     * @param networkMessage Any object to send
     */
    private void sendMessage(PeerAddress receiver, final NetworkMessage networkMessage){
        RequestP2PConfiguration requestP2PConfiguration = new RequestP2PConfiguration(1, 10, 0);
        SendBuilder sendBuilder = peer.send(receiver.getID());

        FutureDHT futureDHT = sendBuilder.setObject( networkMessage.encrypt() ).setRequestP2PConfiguration(requestP2PConfiguration).start();
        futureDHT.addListener(new BaseFutureAdapter<BaseFuture>() {
            @Override
            public void operationComplete(BaseFuture future) throws Exception {
                if(!future.isSuccess()){
                    System.out.println("Error sending " + networkMessage.toString());
                    System.out.println("WHY: "+future.getFailedReason());
                    return;
                }
                System.out.println("Success sending " + networkMessage.toString());
            }
        });

    }
}
