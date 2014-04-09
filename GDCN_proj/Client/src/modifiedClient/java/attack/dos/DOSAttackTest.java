package attack.dos;

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

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.concurrent.Semaphore;

/**
 * Created by HalfLeif on 2014-04-09.
 */
public class DOSAttackTest {

    private Peer[] peers;
    private TaskPasserDOS[] taskPassers;

    private Semaphore boots = new Semaphore(0);
    private Semaphore challenges = new Semaphore(0);

    private OnReplyCommand challengeReceived = new OnReplyCommand() {
        @Override
        public void execute(Object replyMessageContent) {
            challenges.release();
        }
    };

    public static void main(String[] args){
        DOSAttackTest dosAttackTest = new DOSAttackTest();
        dosAttackTest.attackTest();
    }

    public void attackTest(){
        final int sybils = 5;
        peers = new Peer[sybils];
        try {
            for(int ix=0; ix<sybils; ++ix){
                peers[ix] = createPeer(13000+ix);
                taskPassers[ix] = new TaskPasserDOS(peers[ix]);
            }

            for(int ix=0; ix<sybils; ++ix){
                bootstrap(peers[ix], "narrens.olf.sgsnet.se", 4001);
            }
            boots.acquireUninterruptibly((sybils*3)/4);

            final Date startTime = new Date();
            for(int ix=0; ix<sybils; ++ix){
                PeerAddress targetOwner = peers[ix].getPeerBean().getPeerMap().getAll().get(0);
                taskPassers[ix].requestChallenge(targetOwner, challengeReceived);
            }
            challenges.acquireUninterruptibly((sybils*3)/4);
            final Date stopTime = new Date();

            System.out.println("Time it took: "+(stopTime.getTime()-startTime.getTime()));
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            for(int ix=0; ix<sybils; ++ix){
                peers[ix].shutdown();
            }
        }

    }

    private void bootstrap(final Peer peer, String address, final int port){
        try {
            final InetAddress inetAddress = InetAddress.getByName(address);

            DiscoverBuilder discoverBuilder = peer.discover().setInetAddress(inetAddress).setPorts(port);
            FutureDiscover futureDiscover = discoverBuilder.start();


            futureDiscover.addListener(new BaseFutureAdapter<FutureDiscover>() {
                @Override
                public void operationComplete(FutureDiscover future) throws Exception {
                    if(!future.isSuccess()){
                        boots.release();
                        return;
                    }

                    BootstrapBuilder bootstrapBuilder = peer.bootstrap().setInetAddress(inetAddress).setPorts(port);
                    FutureBootstrap futureBootstrap = bootstrapBuilder.start();

                    futureBootstrap.addListener(new BaseFutureAdapter<FutureBootstrap>() {
                        @Override
                        public void operationComplete(FutureBootstrap future) throws Exception {
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
