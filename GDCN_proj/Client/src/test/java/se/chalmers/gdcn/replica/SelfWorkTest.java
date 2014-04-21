package se.chalmers.gdcn.replica;

import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.control.TaskManager;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
import utils.TaskHolder;
import utils.TestUtils;
import utils.WorkerHolder;

import java.beans.PropertyChangeListener;
import java.util.List;
import java.util.concurrent.Semaphore;

/**
 * Created by Leif on 2014-04-21.
 */
public class SelfWorkTest {
    private ReplicaManager replicaManager;
    private Semaphore lock;

    @BeforeMethod
    public void setupMethod(){
        ReplicaManagerBuilder builder = new ReplicaManagerBuilder(WorkerHolder.getMyWorkerID(), emptyTaskManager());

        replicaManager = builder.create();
        replicaManager.setWorkSelfIfRequired(true);
        TestUtils.loadMeta(TaskHolder.getTaskA(), replicaManager);

        lock = new Semaphore(0);
    }

    @Test
    public void oneTest(){
        ReplicaBox replicaBox = replicaManager.giveReplicaToWorker(WorkerHolder.getWorkerA());
        replicaManager.replicaOutdated(replicaBox.getReplicaID());
        //...

        lock.acquireUninterruptibly();
    }

    private TaskManager emptyTaskManager(){
        return new TaskManager(new TaskListener() {
            @Override
            public void taskFinished(String taskName) {
                lock.release();
            }

            @Override
            public void taskFailed(String taskName, String reason) {
                lock.release();
                throw new AssertionError(reason);
            }
        }, new ClientInterface() {
            @Override
            public void install() {

            }

            @Override
            public void uninstall() {

            }

            @Override
            public void push(String jobName) {

            }

            @Override
            public void work(String address, int port) {

            }

            @Override
            public void addListener(PropertyChangeListener listener) {

            }

            @Override
            public void removeListener(PropertyChangeListener listener) {

            }

            @Override
            public void start(int port) {

            }

            @Override
            public void stop() {

            }

            @Override
            public void bootstrap(String host, int port) {

            }

            @Override
            public void put(String name, Data value) {

            }

            @Override
            public void put(Number160 key, Data value) {

            }

            @Override
            public void get(String name) {

            }

            @Override
            public void get(Number160 key) {

            }

            @Override
            public List<PeerAddress> getNeighbours() {
                return null;
            }

            @Override
            public List<PeerAddress> getOldNeighbours() {
                return null;
            }

            @Override
            public void reBootstrap() {

            }

            @Override
            public void send(String msg) {

            }

            @Override
            public void put2(String key, String domain, Object value) {

            }

            @Override
            public void get2(String key, String domain) {

            }

            @Override
            public void setNeighbourFile(String file) {

            }

            @Override
            public void clearNeighbourFile() {

            }

            @Override
            public void deleteNeighbourFile() {

            }

            @Override
            public void requestWork(int index) {

            }
        });
    }
}
