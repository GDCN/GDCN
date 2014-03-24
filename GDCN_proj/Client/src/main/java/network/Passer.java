package network;

import net.tomp2p.futures.BaseFuture;
import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureDHT;
import net.tomp2p.futures.FutureResponse;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.RequestP2PConfiguration;
import net.tomp2p.p2p.builder.SendBuilder;
import net.tomp2p.p2p.builder.SendDirectBuilder;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.rpc.ObjectDataReply;
import net.tomp2p.rpc.RawDataReply;
import org.jboss.netty.buffer.ChannelBuffer;

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
                System.out.println("ObjectDataReply:" + request.toString());
                return null;
            }
        });

        peer.setRawDataReply(new RawDataReply() {
            @Override
            public ChannelBuffer reply(PeerAddress sender, ChannelBuffer requestBuffer) throws Exception {
                System.out.println("Received raw data reply from "+sender);
                return null;
            }
        });
    }

    /**
     * In testing, the message gets through but the Future says not successful...
     * Perhaps has something to do with the reply... TODO check if return "OK" changes that
     * TODO make good message passing protocol for Tasks
     * @param receiver other peer
     * @param message Any object to send
     */
    public void send(PeerAddress receiver, final Object message){
        RequestP2PConfiguration requestP2PConfiguration = new RequestP2PConfiguration(1, 10, 0);
        SendBuilder sendBuilder = peer.send(receiver.getID());

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

    /**
     * SendDirect
     * @param receiver r
     * @param message m
     */
    public void sendd(PeerAddress receiver, final Object message){
//        RequestP2PConfiguration requestP2PConfiguration = new RequestP2PConfiguration(1, 10, 0);
        SendDirectBuilder sendDirectBuilder = peer.sendDirect(receiver);
        sendDirectBuilder.setObject(message);
        FutureResponse futureResponse = sendDirectBuilder.start();
        futureResponse.addListener(new BaseFutureAdapter<BaseFuture>() {
            @Override
            public void operationComplete(BaseFuture future) throws Exception {
                if(!future.isSuccess()){
                    System.out.println("Error sendDing "+message.toString());
                    return;
                }
                System.out.println("Success sendDing "+message.toString());
            }
        });
    }
}
