package domainTests;

import net.tomp2p.futures.FutureDHT;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.p2p.RequestP2PConfiguration;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import net.tomp2p.storage.StorageGeneric;
import net.tomp2p.utils.Utils;
import org.apache.log4j.Logger;
import org.apache.log4j.varia.NullAppender;

import java.io.IOException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Created by Niklas on 2014-04-08.
 */
public class ProblemTests {

    private static Peer[] peers;
    private static Number160 peerOwner;

    private static KeyPair k;
    private static KeyPair k1;

    public static void main(String[] arg) throws IOException, NoSuchAlgorithmException, InterruptedException, ClassNotFoundException {
        Logger.getRootLogger().removeAllAppenders();
        Logger.getRootLogger().addAppender(new NullAppender());

        int calls = 100;
        Boolean[] results = new Boolean[calls];

        findKeys();

//        for(int i = 10; i < calls; i++) {
//            results[i] = masterStaysUp(calls, i);
//            System.out.println("number of Sybils: " + i + " number of peers: " + calls +  " Sybils succedded?: " + results[i]);
//        }
//        System.out.println("Number of seconds: " + bothGoesDown());

//        uploaderRejoin();
//        masterrRejoin();


    }

    private static void findKeys() throws IOException, NoSuchAlgorithmException, InterruptedException {
        initialize(3, 5000, 0, 2);

        FutureDHT futurePut =
                peers[0].put(Number160.ONE).setData( new Data( "success" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        Thread.sleep(1000);

        System.out.println(futurePut.isSuccess());

        futurePut =
                peers[1].put(Number160.ONE).setData( new Data( "success" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        Thread.sleep(1000);

        System.out.println(futurePut.isSuccess());

        futurePut =
                peers[2].put(Number160.ONE).setData( new Data( "success" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        Thread.sleep(1000);

        System.out.println(futurePut.isSuccess());

        futurePut =
                peers[0].put(Number160.ONE).setData( new Data( "success" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        Thread.sleep(1000);

        System.out.println(futurePut.isSuccess());

        shutdown(peers);
    }



    private static int masterGoesDown() throws IOException, NoSuchAlgorithmException, InterruptedException, ClassNotFoundException {
        initialize(3,4000, 1, 2);

        FutureDHT futurePut =
                peers[1].put(Number160.ONE).setData( new Data( "success" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        peers[2].shutdown();

        Thread.sleep(1000);

        futurePut =
                peers[0].put(Number160.ONE).setData( new Data( "Attack" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        if(futurePut.isSuccess()) {
            return 0;
        }

        long time = System.currentTimeMillis();
        int number = 0;
        FutureDHT futureGet;

        while(true) {
            futurePut =
                    peers[0].put(Number160.ONE).setData( new Data( "Attack" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
            futurePut.awaitUninterruptibly();

            futureGet =
                    peers[1].get(Number160.ONE).setDomainKey( peerOwner ).setRequestP2PConfiguration(new RequestP2PConfiguration(20, 10, 0)).start();
            futureGet.awaitUninterruptibly();

            if("Attack".equals(futureGet.getData().getObject())) {
                System.out.println(futurePut.isSuccess());
                shutdown(peers);
                return number * 10;
            }

            if((System.currentTimeMillis() - time) /1000 > 1000) {
                shutdown(peers);
                return 1;
            }

            System.out.println("Current time: " + number * 10);

            number++;

            Thread.sleep(1000 * 10);

        }

    }

    private static int uploaderGoesDown() throws IOException, NoSuchAlgorithmException, InterruptedException, ClassNotFoundException {
        initialize(3,6010, 1, 2);

        FutureDHT futurePut =
                peers[1].put(Number160.ONE).setData( new Data( "success" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        peers[1].shutdown();

        Thread.sleep(1000);

        futurePut =
                peers[0].put(Number160.ONE).setData( new Data( "Attack" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        if(futurePut.isSuccess()) {
            return 0;
        }

        long time = System.currentTimeMillis();
        int number = 0;
        FutureDHT futureGet;

        while(true) {
            futurePut =
                    peers[0].put(Number160.ONE).setData( new Data( "Attack" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
            futurePut.awaitUninterruptibly();

            futureGet =
                    peers[2].get(Number160.ONE).setDomainKey( peerOwner ).setRequestP2PConfiguration(new RequestP2PConfiguration(20, 10, 0)).start();
            futureGet.awaitUninterruptibly();

            if("Attack".equals(futureGet.getData().getObject())) {
                System.out.println(futurePut.isSuccess());
                shutdown(peers);
                return number * 10;
            }

            if((System.currentTimeMillis() - time) /1000 > 1000) {
                shutdown(peers);
                return 1;
            }

            System.out.println("Current time: " + number * 10);

            number++;

            Thread.sleep(1000 * 10);

        }

    }

    private static int bothGoesDown() throws IOException, NoSuchAlgorithmException, InterruptedException, ClassNotFoundException {
        initialize(4,6030, 1, 2);

        FutureDHT futurePut =
                peers[1].put(Number160.ONE).setData( new Data( "success" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        peers[1].shutdown();
        peers[2].shutdown();

        Thread.sleep(1000);

        futurePut =
                peers[0].put(Number160.ONE).setData( new Data( "Attack" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        if(futurePut.isSuccess()) {
            return 0;
        }

        long time = System.currentTimeMillis();
        int number = 0;
        FutureDHT futureGet;

        while(true) {
            futurePut =
                    peers[0].put(Number160.ONE).setData( new Data( "Attack" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
            futurePut.awaitUninterruptibly();

            futureGet =
                    peers[0].get(Number160.ONE).setDomainKey( peerOwner ).setRequestP2PConfiguration(new RequestP2PConfiguration(10, 10, 0)).start();
            futureGet.awaitUninterruptibly();

            if("Attack".equals(futureGet.getData().getObject())) {
                System.out.println(futurePut.isSuccess());
                shutdown(peers);
                return number * 10;
            }

            if((System.currentTimeMillis() - time) /1000 > 1000) {
                shutdown(peers);
                return 1;
            }

            System.out.println("Current time: " + number * 10);

            number++;

            Thread.sleep(1000 * 10);

        }

    }

    private static boolean masterStaysUp(int nP, int nS) throws IOException, NoSuchAlgorithmException, ClassNotFoundException, InterruptedException {

        int peerO = nP-1;

        initialize(nP, 4200, nS, peerO);

        FutureDHT futurePut =
                peers[peerO].put(Number160.ONE).setData( new Data( "test" ) ).setDomainKey( peerOwner ).setProtectDomain().setRequestP2PConfiguration(new RequestP2PConfiguration(6, 10, 0)).start();
        futurePut.awaitUninterruptibly();

        if(nS > 0) {
            futurePut =
                    peers[nS-1].put(Number160.ONE).setData( new Data( "ATTACK" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
            futurePut.awaitUninterruptibly();
        } else {
            futurePut =
                    peers[0].put(Number160.ONE).setData( new Data( "ATTACK" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
            futurePut.awaitUninterruptibly();
        }

        Thread.sleep(1000);

        FutureDHT futureGet = peers[peerO].get( Number160.ONE ).setDomainKey( peerOwner ).setRequestP2PConfiguration(new RequestP2PConfiguration(5,10,0)).start();
        futureGet.awaitUninterruptibly();

        Object o = futureGet.getData().getObject();

//        if("ATTACK".equals(o)) {
//            for (Map.Entry<PeerAddress, Map<Number160, Data>> entry : futureGet.getRawData().entrySet()) {
//                System.out.print("got from (6)" + entry.getKey());
//                System.out.println(entry.getValue());
//            }
//        }
        shutdown(peers);

        return "ATTACK".equals(o);
    }

    private static void uploaderRejoin() throws IOException, NoSuchAlgorithmException, InterruptedException, ClassNotFoundException {
        initialize(3, 3000, 0, 2);

        FutureDHT futurePut =
                peers[1].put(Number160.ONE).setData( new Data( "test" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        System.out.println(futurePut.isSuccess());

        futurePut =
                peers[0].put(Number160.ONE).setData( new Data( "ATTACK" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        System.out.println(futurePut.isSuccess());

        shutdown(peers[1]);

        Thread.sleep(1000);

        futurePut =
                peers[0].put(Number160.ONE).setData( new Data( "ATTACK" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        System.out.println(futurePut.isSuccess());

        peers[1] = new PeerMaker( k ).setPorts(3001).makeAndListen();

        Thread.sleep(1000);

        ExampleUtils.bootstrap(peers);

        futurePut =
                peers[0].put(Number160.ONE).setData( new Data( "ATTACK" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        System.out.println(futurePut.isSuccess());

        Thread.sleep(1000);

        FutureDHT futureGet =
                peers[2].get(Number160.ONE).setDomainKey( peerOwner ).setRequestP2PConfiguration(new RequestP2PConfiguration(20, 10, 0)).start();
        futureGet.awaitUninterruptibly();
        System.out.println(futureGet.getData().getObject());

        futurePut =
                peers[1].put(Number160.ONE).setData( new Data( "test2" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        Thread.sleep(1000);

        futureGet =
                peers[2].get(Number160.ONE).setDomainKey( peerOwner ).setRequestP2PConfiguration(new RequestP2PConfiguration(20, 10, 0)).start();
        futureGet.awaitUninterruptibly();

        System.out.println(futurePut.isSuccess());

        System.out.println(futureGet.getData().getObject());

        System.out.println();

        shutdown(peers);

    }

    private static void masterrRejoin() throws IOException, NoSuchAlgorithmException, InterruptedException, ClassNotFoundException {
        initialize(3, 3000, 0, 2);

        FutureDHT futurePut =
                peers[2].put(Number160.ONE).setData( new Data( "test" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        System.out.println(futurePut.isSuccess());

        futurePut =
                peers[0].put(Number160.ONE).setData( new Data( "ATTACK" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        System.out.println(futurePut.isSuccess());

        shutdown(peers[2]);

        Thread.sleep(1000);

        futurePut =
                peers[0].put(Number160.ONE).setData( new Data( "ATTACK" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        System.out.println(futurePut.isSuccess());

        peers[2] = new PeerMaker( k1 ).setPorts(3002).makeAndListen();

        Thread.sleep(1000);

        ExampleUtils.bootstrap(peers);

        futurePut =
                peers[0].put(Number160.ONE).setData( new Data( "ATTACK" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        System.out.println(futurePut.isSuccess());

        futurePut =
                peers[2].put(Number160.ONE).setData( new Data( "test2" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        System.out.println("OWNER SUCCEDED: " + futurePut.isSuccess());

        Thread.sleep(1000);

        FutureDHT futureGet =
                peers[2].get(Number160.ONE).setDomainKey( peerOwner ).setRequestP2PConfiguration(new RequestP2PConfiguration(20, 10, 0)).start();
        futureGet.awaitUninterruptibly();

        System.out.println(futureGet.getData().getObject());

        shutdown(peers);
    }


    private static Peer createPeer(int port) throws IOException, NoSuchAlgorithmException {
        KeyPairGenerator gen = KeyPairGenerator.getInstance( "DSA" );
        KeyPair pair = gen.generateKeyPair();
        if(port == 3001) {
            k = pair;
        }
        if(port == 3000) {
            k1 = pair;
        }

        return new PeerMaker( pair ).setPorts(port).makeAndListen();
    }

    private static void setProtection( Peer[] peers, StorageGeneric.ProtectionEnable protectionEnable, StorageGeneric.ProtectionMode protectionMode )
    {
        for ( Peer peer : peers )
        {
            peer.getPeerBean().getStorage().setProtection(protectionEnable, protectionMode, protectionEnable,
                    protectionMode);
        }
    }

    private static void setProtection( Peer peer, StorageGeneric.ProtectionEnable protectionEnable, StorageGeneric.ProtectionMode protectionMode )
    {

        peer.getPeerBean().getStorage().setProtection(protectionEnable, protectionMode, protectionEnable,
                protectionMode);

    }

    private static void shutdown(Peer[] ps) {
        for(Peer p : ps) {
            shutdown(p);
        }
    }

    private static void shutdown(Peer p) {
        if(!p.isShutdown()) {
            p.shutdown();
        }
    }

    private static void initialize(int numberPeers, int port, int numberSybil, int peerO) throws IOException, NoSuchAlgorithmException {

        peers = new Peer[numberPeers];

        for(int i = 0; i < numberPeers; i++) {
            peers[i] = createPeer(port+i);
        }

        ExampleUtils.bootstrap(peers);

        peerOwner = Utils.makeSHAHash(peers[peerO].getPeerBean().getKeyPair().getPublic().getEncoded());

//        setProtection(peers, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY);

//        for(int i = 0; i < numberSybil; i++) {
//            setProtection(peers[i], StorageGeneric.ProtectionEnable.NONE, StorageGeneric.ProtectionMode.NO_MASTER );
//        }
    }
}

