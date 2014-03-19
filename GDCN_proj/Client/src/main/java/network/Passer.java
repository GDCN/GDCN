package network;

import net.tomp2p.futures.BaseFuture;
import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureDHT;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.RequestP2PConfiguration;
import net.tomp2p.p2p.builder.SendBuilder;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.rpc.ObjectDataReply;

/**
 * Created by Leif on 2014-03-19.
 */
public class Passer {

    private final Peer peer;

    public Passer(Peer peer) {
        this.peer = peer;
        peer.setObjectDataReply(new ObjectDataReply() {
            @Override
            public Object reply(PeerAddress sender, Object request) throws Exception {
                //TODO
                System.out.println(request.toString());
                return null;
            }
        });
    }

    public void send(PeerAddress receiver, final Object message){
        RequestP2PConfiguration requestP2PConfiguration = new RequestP2PConfiguration(1, 10, 0);
        SendBuilder sendBuilder = peer.send(Number160.createHash("messageKey"));
        FutureDHT futureDHT = sendBuilder.setObject(message).setRequestP2PConfiguration(requestP2PConfiguration).start();
        futureDHT.addListener(new BaseFutureAdapter<BaseFuture>() {
            @Override
            public void operationComplete(BaseFuture future) throws Exception {
                if(!future.isSuccess()){
                    System.out.println("Error sending "+message.toString());
                    return;
                }
                System.out.println("Success sending "+message.toString());
            }
        });

    }
}
