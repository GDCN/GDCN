package attack;

import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.NetworkInterface;
import se.chalmers.gdcn.communicationToUI.Operation;
import se.chalmers.gdcn.communicationToUI.OperationFinishedSupport;
import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureDHT;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import se.chalmers.gdcn.network.DeceitfulNetworkUtils;
import se.chalmers.gdcn.network.OnReplyCommand;
import se.chalmers.gdcn.network.TaskPasserDeny;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.beans.PropertyChangeListener;
import java.util.List;
import java.util.Map;
import java.util.concurrent.*;

/**
 * Created by HalfLeif on 2014-04-09.
 */
public class DenyTaskAttack {

    private Peer[] peers;
    private TaskPasserDeny[] taskPassers;

    private Peer peer;
    private TaskPasserDeny taskPasserDeny;
    private Map<String, byte[]> falseResults = new ConcurrentHashMap<>();
    private ExecutorService pool = Executors.newFixedThreadPool(4);

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

    @BeforeMethod
    public void setupMethod(){

    }

    @Test
    public void denyTest() throws InterruptedException {
        Denier[] deniers = new Denier[4];
        for(int i=0; i<deniers.length; ++i){
            deniers[i] = new Denier(falseResults, i, pool);
        }

        for(int replay=0; replay<3; ++replay){
            Thread.sleep(15000);
            for(int i=0; i<deniers.length; ++i){
                deniers[i].startAgain();
            }
        }

        pool.awaitTermination(100, TimeUnit.SECONDS);
    }

    private static class Denier {
        private Peer peer;
        private TaskPasserDeny taskPasserDeny;
        private final ExecutorService pool;

        private Denier(Map<String, byte[]> falseResults, int index, ExecutorService pool) {
            this.peer = DeceitfulNetworkUtils.createPeer(13000+index);
            this.taskPasserDeny = new TaskPasserDeny(peer, client, falseResults, pool);
            this.pool = pool;

            startAgain();
        }

        /**
         * Called in constructor as well.
         */
        public void startAgain(){
            pool.submit(runnable);
        }

        private Runnable runnable = new Runnable() {

            @Override
            public void run() {
                DeceitfulNetworkUtils.bootstrap(peer, "narrens.olf.sgsnet.se", 4001, new OnReplyCommand() {
                    @Override
                    public void execute(Object replyMessageContent) {
                        System.out.println("\tBootstrap done");

                        PeerAddress jobOwner = (PeerAddress) replyMessageContent;
                        if(jobOwner == null){
                            peer.shutdown();
                        } else {
                            taskPasserDeny.requestWork(jobOwner);
                        }
                    }
                });
            }
        };

        private final NetworkInterface client = new NetworkInterface() {
            private OperationFinishedSupport notifier = new OperationFinishedSupport(this);

            @Override
            public void put(final Number160 key, final Data value) {
                FutureDHT futureDHT = peer.put(key).setData(value).start();
                futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {

                    @Override
                    public void operationComplete(FutureDHT future) throws Exception {
                        boolean success = future.isSuccess();

                        notifier.fireOperationFinished(CommandWord.PUT, new Operation.OperationBuilder<Data>(success).setResult(value).setKey(key).create());
                    }
                });
            }

            @Override
            public void addListener(PropertyChangeListener listener){
                notifier.addListener(listener);
            }

            @Override
            public void removeListener(PropertyChangeListener listener){
                notifier.removeListener(listener);
            }

            //Not supported in this test:
            @Override
            public void put(String name, Data value) {}

            @Override
            public void get(String name) {}

            @Override
            public void get(Number160 key) {}

            @Override
            public void start(int port) {}

            @Override
            public void stop() {}

            @Override
            public void bootstrap(String host, int port) {}

            @Override
            public void bootstrap() {}

            @Override
            public List<PeerAddress> getNeighbours() {return null;}

            @Override
            public List<PeerAddress> getOldNeighbours() {return null;}

            @Override
            public void reBootstrap() {}

            @Override
            public void send(String msg) {}

            @Override
            public void put(Number160 key, Number160 domain, Data value) {

            }

            @Override
            public void get(Number160 key, Number160 domain) {

            }

            @Override
            public void setNeighbourFile(String file) {}

            @Override
            public void clearNeighbourFile() {}

            @Override
            public void deleteNeighbourFile() {}

            @Override
            public void requestWork(int index) {}

            @Override
            public Number160 getID() {
                return null;
            }
        };
    }


}
