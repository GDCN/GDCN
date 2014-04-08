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
            results[i] = masterGoesDown(calls, i);
            System.out.println("number of Sybils: " + i + " number of peers: " + calls +  " Sybils succedded?: " + results[i]);
        }
    }

    private static boolean masterGoesDown(int nP, int nS) throws IOException, NoSuchAlgorithmException, ClassNotFoundException {

        int peerO = 9;
        int numberPeers = nP;
        int numberSybil = nS;

        initialize(numberPeers, 4000, numberSybil, peerO);

        FutureDHT futurePut =
                peers[peerO].put(Number160.ONE).setData( new Data( "test" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        if(numberSybil > 0) {
            futurePut =
                    peers[numberSybil-1].put(Number160.ONE).setData( new Data( "ATTACK" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
            futurePut.awaitUninterruptibly();
        } else {
            futurePut =
                    peers[0].put(Number160.ONE).setData( new Data( "ATTACK" ) ).setDomainKey( peerOwner ).setProtectDomain().start();
            futurePut.awaitUninterruptibly();
        }

        FutureDHT futureGet = peers[peerO].get( Number160.ONE ).setDomainKey( peerOwner ).setRequestP2PConfiguration(new RequestP2PConfiguration(20,10,0)).start();
        futureGet.awaitUninterruptibly();

        shutdown(peers);

        return !"test".equals(futureGet.getData().getObject());


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

