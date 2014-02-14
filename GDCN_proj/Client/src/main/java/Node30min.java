import net.tomp2p.futures.*;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.p2p.builder.DiscoverBuilder;
import net.tomp2p.peers.Number160;
import net.tomp2p.storage.Data;
import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.io.IOException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.Random;

/**
* Created by Leif on 2014-02-13.
*/
public class Node30min {

    private final Peer peer;

    private static Logger logger = Logger.getLogger(Node30min.class);

    public Node30min() throws IOException, NoSuchAlgorithmException {

        final int port = 12001;

        //Random method
        Random random = new Random();
        peer = new PeerMaker(new Number160(random)).setPorts(port).makeAndListen();

        peer.shutdown();

        //KeyPair method
        KeyPairGenerator gen = KeyPairGenerator.getInstance("DSA");
        KeyPair pair1 = gen.generateKeyPair();
        Peer peer1 = new PeerMaker( pair1 ).setPorts(port).makeAndListen();

        //Create sub peer?
        Peer otherPeer = new PeerMaker( gen.generateKeyPair()).setMasterPeer(peer1).makeAndListen();

        //
        DiscoverBuilder discoverBuilder = otherPeer.discover().setPeerAddress(peer1.getPeerAddress());
        FutureDiscover futureDiscover = discoverBuilder.start();
        futureDiscover.awaitUninterruptibly();

        FutureBootstrap futureBootstrap = otherPeer.bootstrap().setPeerAddress(peer1.getPeerAddress()).start();
        futureBootstrap.awaitUninterruptibly();

        //Put Data
        Data data = new Data("Test");
        Number160 nr = new Number160(random);
        FutureDHT futureDHT = peer1.put(nr).setData(data).start();
        futureDHT.awaitUninterruptibly();

        //Get Data with Listener. More safe than await() since no deadlock can happen using listener.
        FutureDHT futureDHTget = peer1.get(nr).start();
        futureDHTget.addListener(new BaseFutureAdapter<FutureDHT>() {
            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                logger.info("Received "+future.getData().getObject().toString());
            }
        });

        //Shutdown should be called by master peer
        try {
            Thread.sleep(2000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        peer1.shutdown();
    }

    public static void main(String[] args){
        BasicConfigurator.configure();
        logger.info("Enter main");
        test();
    }

    public static void test(){
        try {
            Node30min node = new Node30min();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }


    /*
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
    }*/

}
