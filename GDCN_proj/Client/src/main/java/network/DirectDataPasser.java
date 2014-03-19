package network;

import net.tomp2p.connection.PeerConnection;
import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureResponse;
import net.tomp2p.message.Message;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.builder.SendDirectBuilder;
import net.tomp2p.peers.PeerAddress;

/**
 * Created by Leif on 2014-03-19.
 */
public class DirectDataPasser {

    private final Peer peer;

    private final static int TIMEOUT = 1500;

    public DirectDataPasser(Peer peer) {
        this.peer = peer;
    }

    public void sendObject(Object o, PeerAddress receiver){
        PeerConnection peerConnection = peer.createPeerConnection(receiver, TIMEOUT);
        SendDirectBuilder builder = peer.sendDirect(peerConnection).setObject(o);
        FutureResponse futureResponse = builder.start();

        futureResponse.addListener(new BaseFutureAdapter<FutureResponse>() {
            @Override
            public void operationComplete(FutureResponse future) throws Exception {
                if(!future.isSuccess()){
                    //TODO output
                    return;
                }
                Message response = future.getResponse();
                //TODO output
            }
        });

    }
}
