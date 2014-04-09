package network;

import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;

import java.io.IOException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

/**
 * Created by HalfLeif on 2014-04-09.
 */
public class DeceitfulNetworkUtils {
    public static Peer createPeer(int port){
        KeyPairGenerator generator = null;
        try {
            generator = KeyPairGenerator.getInstance("RSA");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
        assert generator != null;
        KeyPair keyPair = generator.generateKeyPair();

        try {
            return new PeerMaker(keyPair).setPorts(port).makeAndListen();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}
