package domainTests;

import net.tomp2p.futures.FutureDHT;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.p2p.RequestP2PConfiguration;
import net.tomp2p.peers.Number160;
import net.tomp2p.storage.Data;
import net.tomp2p.storage.StorageGeneric;
import net.tomp2p.utils.Utils;
import org.apache.log4j.Logger;
import org.apache.log4j.varia.NullAppender;

import java.io.IOException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

/**
 * Created by Niklas on 2014-04-08.
 */
public class ProblemTests {

    private static Peer[] peers;
    private static Number160 peerOwner;

    public static void main(String[] arg) throws IOException, NoSuchAlgorithmException, InterruptedException, ClassNotFoundException {
        Logger.getRootLogger().removeAllAppenders();
        Logger.getRootLogger().addAppender(new NullAppender());

        int calls = 100;
        Boolean[] results = new Boolean[calls];

        for(int i = 10; i < calls; i++) {
            results[i] = masterStaysUp(calls, i);
            System.out.println("number of Sybils: " + i + " number of peers: " + calls +  " Sybils succedded?: " + results[i]);
        }
//        System.out.println("Number of seconds: " + bothGoesDown());



    }

    private static int masterGoesDown() throws IOException, NoSuchAlgorithmException, InterruptedException, ClassNotFoundException {
        initialize(3,6000, 1, 2);

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
                    peers[0].get(Number160.ONE).setDomainKey( peerOwner ).setRequestP2PConfiguration(new RequestP2PConfiguration(20, 10, 0)).start();
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

    private static boolean masterStaysUp(int nP, int nS) throws IOException, NoSuchAlgorithmException, ClassNotFoundException {

        int peerO = nP-1;

        initialize(nP, 4000, nS, peerO);

        FutureDHT futurePut =
                peers[peerO].put(Number160.ONE).setData( new Data( "test" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
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

        FutureDHT futureGet = peers[peerO].get( Number160.ONE ).setDomainKey( peerOwner ).setRequestP2PConfiguration(new RequestP2PConfiguration(20,10,0)).start();
        futureGet.awaitUninterruptibly();

        shutdown(peers);

        return "ATTACK".equals(futureGet.getData().getObject());


    }


    private static Peer createPeer(int port) throws IOException, NoSuchAlgorithmException {
        KeyPairGenerator gen = KeyPairGenerator.getInstance( "DSA" );
        KeyPair pair = gen.generateKeyPair();
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

        setProtection(peers, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY);

        for(int i = 0; i < numberSybil; i++) {
            setProtection(peers[i], StorageGeneric.ProtectionEnable.NONE, StorageGeneric.ProtectionMode.NO_MASTER );
        }
    }
}

