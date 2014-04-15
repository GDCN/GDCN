package attack;

import se.chalmers.gdcn.hashcash.HashCash;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.PeerAddress;
import se.chalmers.gdcn.network.DeceitfulNetworkUtils;
import se.chalmers.gdcn.network.OnReplyCommand;
import se.chalmers.gdcn.network.TaskPasserDOS;
import se.chalmers.gdcn.network.WorkerID;
import org.testng.annotations.Test;

import javax.crypto.KeyGenerator;
import java.security.InvalidKeyException;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.concurrent.Semaphore;

/**
 * Created by HalfLeif on 2014-04-09.
 *
 * Conclusion: It seems to be more expensive to generate a new Sybil node than to create a new Challenge!
 *
 * This attack is ineffective but if it was to be continued:
 * TODO use FutureDiscover to get PeerAddress of jobOwner instead of PeerBean
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
    final private OnReplyCommand bootstrapDone = new OnReplyCommand() {
        @Override
        public void execute(Object replyMessageContent) {
            boots.release();
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
        Peer peer = DeceitfulNetworkUtils.createPeer(26789);
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
        Peer peer = DeceitfulNetworkUtils.createPeer(17677);
        TaskPasserDOS taskPasserDOS = new TaskPasserDOS(peer);
        final int messages = 500;

        try {
            DeceitfulNetworkUtils.bootstrap(peer, "narrens.olf.sgsnet.se", 4001, bootstrapDone);
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
                peers[ix] = DeceitfulNetworkUtils.createPeer(13000 + ix);
                taskPassers[ix] = new TaskPasserDOS(peers[ix]);
            }


            for(int ix=0; ix<sybils; ++ix){
                DeceitfulNetworkUtils.bootstrap(peers[ix], "narrens.olf.sgsnet.se", 4001, bootstrapDone);
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

}
