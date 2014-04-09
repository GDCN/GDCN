package attack.dos;

import hashcash.HashCash;
import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureBootstrap;
import net.tomp2p.futures.FutureDiscover;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.p2p.builder.BootstrapBuilder;
import net.tomp2p.p2p.builder.DiscoverBuilder;
import net.tomp2p.peers.PeerAddress;
import network.OnReplyCommand;
import network.TaskPasserDOS;
import network.WorkerID;
import org.testng.annotations.Test;

import javax.crypto.KeyGenerator;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.InvalidKeyException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.concurrent.Semaphore;

/**
 * Created by HalfLeif on 2014-04-09.
 *
 * Conclusion: It seems to be more expensive to generate a new Sybil node than to create a new Challenge!
 */
public class DOSAttackTest {

    private Peer[] peers;
    private TaskPasserDOS[] taskPassers;

    private Semaphore boots = new Semaphore(0);
    private Semaphore challenges = new Semaphore(0);

    final private OnReplyCommand challengeReceived = new OnReplyCommand() {
        @Override
        public void execute(Object replyMessageContent) {
            challenges.release();
        }
    };

    @Test
    public static void timeDiff() throws NoSuchAlgorithmException, InvalidKeyException {
        KeyGenerator keyGenerator = KeyGenerator.getInstance("HmacSHA256");
        HashCash hashCash = new HashCash(keyGenerator.generateKey());

        KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
        WorkerID workerIDa = new WorkerID(keyPairGenerator.generateKeyPair().getPublic());
        WorkerID workerIDb = new WorkerID(keyPairGenerator.generateKeyPair().getPublic());

        Date beforeChallenge = new Date();
        hashCash.generateRegistrationChallenge(workerIDa, workerIDb);
        Date afterChallenge = new Date();

        final long challengeDiff = afterChallenge.getTime()-beforeChallenge.getTime();
        System.out.println("Create challenge: "+challengeDiff);

        Date beforePeer = new Date();
        Peer peer = createPeer(26789);
        Date afterPeer = new Date();

        final long peerDiff = afterPeer.getTime()-beforePeer.getTime();
        System.out.println("Create peer: "+peerDiff);

        assert challengeDiff < peerDiff;
    }

    public static void main(String[] args){
        DOSAttackTest dosAttackTest = new DOSAttackTest();
//        dosAttackTest.attackTest();
        dosAttackTest.reAttackTest();
    }

    public void reAttackTest(){
        Peer peer = createPeer(17677);
        TaskPasserDOS taskPasserDOS = new TaskPasserDOS(peer);
        final int messages = 500;

        try {
            bootstrap(peer, "narrens.olf.sgsnet.se", 4001);
//            boots.acquireUninterruptibly();
            Thread.sleep(100);

            final PeerAddress jobOwner = peer.getPeerBean().getPeerMap().getAll().get(0);

            Date beforePeer = new Date();
            for(int i=0; i<messages; ++i){
                taskPasserDOS.requestChallenge(jobOwner, challengeReceived);
            }
            Date afterPeer = new Date();

            final long peerDiff = afterPeer.getTime()-beforePeer.getTime();
            System.out.println("\tSend messages: "+peerDiff);


            Date beforeChallenge = new Date();
            challenges.acquireUninterruptibly(messages);
            Date afterChallenge = new Date();

            final long challengeDiff = afterChallenge.getTime()-beforeChallenge.getTime();
            System.out.println("\tReceive challenges: "+challengeDiff);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            peer.shutdown();
        }


    }

    public void attackTest(){
        final int sybils = 50;
        peers = new Peer[sybils];
        taskPassers = new TaskPasserDOS[sybils];

        try {
            for(int ix=0; ix<sybils; ++ix){
                peers[ix] = createPeer(13000+ix);
                taskPassers[ix] = new TaskPasserDOS(peers[ix]);
            }

            for(int ix=0; ix<sybils; ++ix){
                bootstrap(peers[ix], "narrens.olf.sgsnet.se", 4001);
            }
            System.out.println("\tAwait bootstrap");
//            boots.acquireUninterruptibly((sybils * 3) / 4);
            Thread.sleep(500);

            final Date startTime = new Date();
            for(int ix=0; ix<sybils; ++ix){
                PeerAddress targetOwner = peers[ix].getPeerBean().getPeerMap().getAll().get(0);
                taskPassers[ix].requestChallenge(targetOwner, challengeReceived);
            }
            System.out.println("\tAwait challenges");
//            challenges.acquireUninterruptibly((sybils*3)/4);
            challenges.acquireUninterruptibly(sybils-1);
            final Date stopTime = new Date();

            System.out.println("Time it took: "+(stopTime.getTime()-startTime.getTime()));
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            for(int ix=0; ix<sybils; ++ix){
                if(peers[ix]==null){
                    continue;
                }
                peers[ix].shutdown();
            }
        }

    }

    private void bootstrap(final Peer peer, String address, final int port){
        try {
            final InetAddress inetAddress = InetAddress.getByName(address);

            DiscoverBuilder discoverBuilder = peer.discover().setInetAddress(inetAddress).setPorts(port);
            discoverBuilder.start().addListener(new BaseFutureAdapter<FutureDiscover>() {
                @Override
                public void operationComplete(FutureDiscover future) throws Exception {
                    if (!future.isSuccess()) {
                        System.out.println("Bootstrap insuccessful");
                        boots.release();
                        return;
                    }

                    BootstrapBuilder bootstrapBuilder = peer.bootstrap().setInetAddress(inetAddress).setPorts(port);
                    bootstrapBuilder.start().addListener(new BaseFutureAdapter<FutureBootstrap>() {
                        @Override
                        public void operationComplete(FutureBootstrap future) throws Exception {
                            System.out.println("Bootstrap successful?");
                            boots.release();
                        }
                    });
                }
            });
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
    }

    private static Peer createPeer(int port){
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
