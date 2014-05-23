package se.chalmers.gdcn.deceitful;

import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.PeerAddress;

/**
 * Created by HalfLeif on 2014-05-23.
 */
public class SpamWork implements DeceitfulWork{

    private final int times;
    private final Peer[] peers;

    public SpamWork(int times) {
        this.times = times;
        peers = new Peer[times];
    }

    @Deceitful
    @Override
    public void requestWork(PeerAddress jobOwner) {
        
    }
}
