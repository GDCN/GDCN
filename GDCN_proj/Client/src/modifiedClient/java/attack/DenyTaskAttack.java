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
import network.TaskPasserDeny;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.beans.PropertyChangeListener;
import java.util.List;

/**
 * Created by HalfLeif on 2014-04-09.
 */
public class DenyTaskAttack {

    private Peer peer;
    private TaskPasserDeny taskPasserDeny;


    @BeforeMethod
    public void setupMethod(){
        peer = DeceitfulNetworkUtils.createPeer(12674);
    }

    @Test
    public void denyTest(){
        taskPasserDeny = new TaskPasserDeny(peer,client);
    }

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
