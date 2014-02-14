import net.tomp2p.futures.FutureBootstrap;
import net.tomp2p.futures.FutureDHT;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.p2p.builder.DiscoverBuilder;
import net.tomp2p.p2p.builder.PutBuilder;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.io.IOException;
import java.util.Iterator;

/**
* Created by Leif on 2014-02-13.
*/
public class Node5min {

    private final Peer peer;

    private static Logger logger = Logger.getLogger(Node5min.class);

    public Node5min(int peerId) throws IOException {
        peer = new PeerMaker(Number160.createHash(peerId)).setPorts(9000+peerId).makeAndListen();

        FutureBootstrap fb = peer.bootstrap().setBroadcast().setPorts(9001).start();

        fb.awaitUninterruptibly();
        if(fb.getBootstrapTo() != null){
            Iterator<PeerAddress> iterator = fb.getBootstrapTo().iterator();
            DiscoverBuilder discoverBuilder = peer.discover().setPeerAddress(iterator.next());
            discoverBuilder.start().awaitUninterruptibly();
        }
    }

    public void stop(){
        peer.shutdown();
    }

    private String get(String name) throws IOException, ClassNotFoundException {
        FutureDHT futureDHT = peer.get(Number160.createHash(name)).start();
        futureDHT.awaitUninterruptibly();
        if(futureDHT.isSuccess()){
            return futureDHT.getData().getObject().toString();
        }
        return "not found";
    }

    private void store(String name, String ip) throws IOException {
        PutBuilder putBuilder = peer.put(Number160.createHash(name));
        putBuilder.setData(new Data(ip)).start().awaitUninterruptibly();
    }

    public static void main(String[] args){
        BasicConfigurator.configure();
        logger.info("Enter main");
        test();
    }

    public static void test(){
        int peerId = 1;
        String key = "some key";
        String value = "another value";
        logger.info("Main test start");

        try {
            Node5min node = node = new Node5min(peerId);
            node.store(key, value);
            String ip = node.get(key);

            logger.info("Stored under "+key);
            logger.info("Original: "+value+" found "+ip);

            node.stop();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

}
