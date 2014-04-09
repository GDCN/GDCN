package attack;

import command.communicationToUI.CommandWord;
import command.communicationToUI.NetworkInterface;
import command.communicationToUI.Operation;
import command.communicationToUI.OperationFinishedSupport;
import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureDHT;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import network.DeceitfulNetworkUtils;
import network.OnReplyCommand;
import network.TaskPasserDeny;
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
        for(int i=0; i<4; ++i){
            new Denier(falseResults, i, pool);
        }
        pool.awaitTermination(100, TimeUnit.SECONDS);
    }

    private static class Denier {
        private Peer peer;
        private TaskPasserDeny taskPasserDeny;

        private Denier(Map<String, byte[]> falseResults, int index, ExecutorService pool) {
            this.peer = DeceitfulNetworkUtils.createPeer(13000+index);
            taskPasserDeny = new TaskPasserDeny(peer, client, falseResults, pool);
            pool.submit(runnable);
        }

        private Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    final Semaphore bootstrap = new Semaphore(0);
                    DeceitfulNetworkUtils.bootstrap(peer, "narrens.olf.sgsnet.se", 4001, new OnReplyCommand() {
                        @Override
                        public void execute(Object replyMessageContent) {
                            bootstrap.release();
                        }
                    });
                    bootstrap.acquireUninterruptibly();
                    System.out.println("\tBootstrap done");
                    taskPasserDeny.requestWork(peer.getPeerBean().getPeerMap().getAll().get(0));

                } catch (Exception e) {
                    e.printStackTrace();
                } finally {
                    //TODO shutdown peer after finish...
//                    peer.shutdown();
                }
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
            public List<PeerAddress> getNeighbours() {return null;}

            @Override
            public List<PeerAddress> getOldNeighbours() {return null;}

            @Override
            public void reBootstrap() {}

            @Override
            public void send(String msg) {}

            @Override
            public void put2(String key, String domain, Object value) {}

            @Override
            public void get2(String key, String domain) {}

            @Override
            public void setNeighbourFile(String file) {}

            @Override
            public void clearNeighbourFile() {}

            @Override
            public void deleteNeighbourFile() {}

            @Override
            public void requestWork(int index) {}
        };
    }


}
