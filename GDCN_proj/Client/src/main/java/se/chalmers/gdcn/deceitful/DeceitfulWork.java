package se.chalmers.gdcn.deceitful;

import net.tomp2p.peers.PeerAddress;

/**
 * Created by HalfLeif on 2014-05-23.
 */
public interface DeceitfulWork {
    @Deceitful
    public void requestWork(PeerAddress jobOwner);
}
