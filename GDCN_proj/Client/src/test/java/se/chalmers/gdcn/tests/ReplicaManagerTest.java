package se.chalmers.gdcn.tests;

import net.tomp2p.peers.Number160;
import net.tomp2p.storage.Data;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import se.chalmers.gdcn.control.WorkerReputationManager;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.replica.ReplicaBox;
import se.chalmers.gdcn.replica.ReplicaManager;
import se.chalmers.gdcn.replica.ReplicaManager.ReplicaID;
import se.chalmers.gdcn.replica.ReplicaManagerBuilder;
import se.chalmers.gdcn.utils.Time;
import utils.TaskHolder;
import utils.TestUtils;
import utils.WorkerHolder;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.Set;
import java.util.concurrent.Semaphore;

/**
 * Created by Leif on 2014-04-01.
 *
 */
public class ReplicaManagerTest {

    private ReplicaManagerBuilder builder;
    private ReplicaManager replicaManager;
    private WorkerReputationManager workerReputationManager;

    private TaskMeta taskMetaA;
    private TaskMeta taskMetaB;

    private WorkerID workerA;
    private WorkerID workerB;
    private WorkerID workerC;
    private WorkerID workerD;

    private WorkerID myWorkerID;

    @BeforeClass
    public void setupClass() {
        taskMetaA = TaskHolder.getTaskA();
        taskMetaB = TaskHolder.getTaskB();

        workerA = WorkerHolder.getWorkerA();
        workerB = WorkerHolder.getWorkerB();
        workerC = WorkerHolder.getWorkerC();
        workerD = WorkerHolder.generate();
        myWorkerID = WorkerHolder.getMyWorkerID();
    }

    @BeforeMethod
    public void setupMethod(){
        workerReputationManager = new WorkerReputationManager(myWorkerID);
        workerReputationManager.registerWorker(workerA);
        workerReputationManager.registerWorker(workerB);
        workerReputationManager.registerWorker(workerC);

        builder = new ReplicaManagerBuilder(workerReputationManager);
        builder.setReplicas(2);

        replicaManager = builder.create();
        replicaManager.setWorkSelfIfRequired(false);
    }

    @Test
    public void dataTest(){
        assert taskMetaA.getTaskName().equals("PrimeTask_01");
        assert taskMetaB.getTaskName().equals("PrimeTask_02");
    }

    @Test
    public void keyTest(){
        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);

        assert ! replicaBoxA.getResultKey().equals(replicaBoxB.getResultKey());
        assert replicaBoxA.getResultKey().equals(replicaManager.getReplicaResultKey(replicaBoxA.getReplicaID()));
        assert replicaBoxB.getResultKey().equals(replicaManager.getReplicaResultKey(replicaBoxB.getReplicaID()));
    }

    @Test
    public void giveWorkTest(){
        assert null == replicaManager.giveReplicaToWorker(workerA);

        loadMeta(taskMetaA);
        assert null != replicaManager.giveReplicaToWorker(workerA);

        //Already has one from this task
        assert null == replicaManager.giveReplicaToWorker(workerA);

        assert null != replicaManager.giveReplicaToWorker(workerB);

        //OBS not anymore since reputation...: No replicas left, only use 2 replicas in this test
//        assert null == replicaManager.giveReplicaToWorker(workerC);
    }

    @Test
    public void multiTest(){
        loadMeta(taskMetaA);
        loadMeta(taskMetaB);

        ReplicaBox replicaBox1 = replicaManager.giveReplicaToWorker(workerA);
        ReplicaBox replicaBox2 = replicaManager.giveReplicaToWorker(workerA);

        assert replicaBox1 != null;
        assert replicaBox2 != null;
        assert ! replicaBox1.getReplicaID().equals(replicaBox2.getReplicaID());

        //There are only two different tasks
        assert null == replicaManager.giveReplicaToWorker(workerA);
    }

    @Test
    public void finishReplicaTest(){
        boolean exceptionThrown = false;
        try{
            replicaManager.replicaFinished(new ReplicaID("NotARealID"), new byte[1]);
        } catch (Exception e){
            exceptionThrown = true;
        }
        assert exceptionThrown;

        loadMeta(taskMetaA);
        ReplicaBox replicaBox = replicaManager.giveReplicaToWorker(workerA);

        boolean exceptionThrown2 = false;
        try{
            replicaManager.replicaFinished(replicaBox.getReplicaID(), null);
        } catch (Exception e){
            exceptionThrown2 = true;
        }
        assert exceptionThrown2;

        replicaManager.replicaFinished(replicaBox.getReplicaID(), new byte[1]);
    }

    @Test
    public void serializeManagerTest() throws IOException, ClassNotFoundException {
        loadMeta(taskMetaA);
        final ReplicaBox replicaBox = replicaManager.giveReplicaToWorker(workerA);

        Data serialized = new Data(replicaManager);
        ReplicaManager replicaManagerCopy = (ReplicaManager) serialized.getObject();

        Number160 originalKey = replicaBox.getResultKey();
        Number160 firstKey = replicaManager.getReplicaResultKey(replicaBox.getReplicaID());
        Number160 secondKey = replicaManagerCopy.getReplicaResultKey(replicaBox.getReplicaID());
        assert originalKey.equals(firstKey);
        assert originalKey.equals(secondKey);

        //Throws exception if doesn't remember workerA:
        replicaManagerCopy.replicaFinished(replicaBox.getReplicaID(), new byte[0]);
        assert null == replicaManagerCopy.giveReplicaToWorker(workerA);
        assert null != replicaManagerCopy.giveReplicaToWorker(workerB);

        //It is truly a deep copy, not just a shallow copy
        assert null != replicaManager.giveReplicaToWorker(workerB);
    }

    @Test
    public void serializeReplicaBoxTest() throws IOException, ClassNotFoundException {
        loadMeta(taskMetaA);
        final ReplicaBox replicaBox = replicaManager.giveReplicaToWorker(workerA);

        Data serialized = new Data(replicaBox);
        ReplicaBox boxCopy = (ReplicaBox) serialized.getObject();

        assert replicaBox.equals(boxCopy);
        assert replicaBox != boxCopy;
    }

    @Test
    public void isAssignedTest(){
        loadMeta(taskMetaA);
        ReplicaID falseReplicaID = new ReplicaID("SomeID");
        assert ! replicaManager.isWorkerAssignedReplica(workerA, falseReplicaID);
        ReplicaBox replicaBox = replicaManager.giveReplicaToWorker(workerA);

        assert replicaManager.isWorkerAssignedReplica(workerA, replicaBox.getReplicaID());
        assert ! replicaManager.isWorkerAssignedReplica(workerB, replicaBox.getReplicaID());
        assert ! replicaManager.isWorkerAssignedReplica(workerA, falseReplicaID);
    }

    @Test
    public void pendingTest(){
        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);

        Set<ReplicaID> pending = replicaManager.pendingReplicaIDs();
        assert pending.size() == 2;
        assert pending.contains(replicaBoxA.getReplicaID());
        assert pending.contains(replicaBoxB.getReplicaID());
    }

    @Test
    public void outdateTimerTest(){
        builder.setTimeoutLength(1, Time.MILLISECOND);
        builder.setTimerUpdateInterval(15, Time.MILLISECOND);
        replicaManager = builder.create();
        replicaManager.setWorkSelfIfRequired(false);

        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        TestUtils.nap(25);
        assert replicaManager.pendingReplicaIDs().size() == 0;
    }

    @Test
    public void replicaFinishTest(){
        builder.setTimeoutLength(1, Time.MILLISECOND);
        replicaManager = builder.create();
        replicaManager.setWorkSelfIfRequired(false);

        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        assert replicaManager.pendingReplicaIDs().contains(replicaBoxA.getReplicaID());

        replicaManager.replicaFinished(replicaBoxA.getReplicaID(), new byte[1]);
        assert replicaManager.pendingReplicaIDs().size() == 0;
    }


    @Test
    public void outdateAfterFinishTest(){
        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);

        replicaManager.replicaFinished(replicaBoxA.getReplicaID(), new byte[1]);

        boolean exceptionThrown = false;
        try {
            replicaManager.replicaOutdated(replicaBoxA.getReplicaID());
        } catch (Exception e) {
            exceptionThrown = true;
        }
        assert exceptionThrown;
    }

    @Test
    public void replicaOutdatedTest(){
        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);
        ReplicaBox replicaBoxC = replicaManager.giveReplicaToWorker(workerC);

        replicaManager.replicaOutdated(replicaBoxA.getReplicaID());

        assert replicaBoxC != null;
        String taskA = replicaBoxA.getTaskMeta().getTaskName();
        String taskB = replicaBoxB.getTaskMeta().getTaskName();
        String taskC = replicaBoxC.getTaskMeta().getTaskName();
        assert taskC.equals(taskA);
        assert taskC.equals(taskB);

        assert ! replicaBoxC.getReplicaID().equals(replicaBoxA.getReplicaID());

        //No exception:
        replicaManager.replicaFinished(replicaBoxA.getReplicaID(), new byte[1]);
        replicaManager.replicaFinished(replicaBoxB.getReplicaID(), new byte[1]);
        replicaManager.replicaFinished(replicaBoxC.getReplicaID(), new byte[1]);

        assert replicaManager.pendingReplicaIDs().size() == 0;
    }

    @Test
    public void serializedTimerTestOnReplicaManager() throws IOException, ClassNotFoundException {
        builder.setTimeoutLength(150, Time.MILLISECOND);
        builder.setTimerUpdateInterval(30, Time.MILLISECOND);
        replicaManager = builder.create();

        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        Data serialized = new Data(replicaManager);

        ReplicaManager deserialized = (ReplicaManager) serialized.getObject();
        deserialized.setWorkSelfIfRequired(false);

        assert deserialized.isWorkerAssignedReplica(workerA, replicaBoxA.getReplicaID());
        assert deserialized.pendingReplicaIDs().contains(replicaBoxA.getReplicaID());

        deserialized.resumeTimer();
        TestUtils.nap(180);
        assert deserialized.pendingReplicaIDs().size() == 0;
    }

    @Test
    public void firstChoiceTest(){
        loadMeta(taskMetaA);
        loadMeta(taskMetaB);

        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);

        //Should indeed work on different tasks since more reputation is required.
        assert ! replicaBoxA.getTaskMeta().getTaskName().equals(replicaBoxB.getTaskMeta().getTaskName());
    }

    @Test
    public void obviousChoiceTest(){
        builder.setExpectedReputation(4);
        replicaManager = builder.create();
        promote(workerA, 4);

        loadMeta(taskMetaA);
        loadMeta(taskMetaB);

        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);

        //Since workerA has high reputation, should work on same task as B which has no reputation
        assert replicaBoxA.getTaskMeta().getTaskName().equals(replicaBoxB.getTaskMeta().getTaskName());
    }

    @Test
    public void smartChoiceTest(){
        builder.setExpectedReputation(4);
        builder.setReplicas(2);
        replicaManager = builder.create();
        promote(workerA, 4);
        //Reputation 4 is the limit in this case since floor() is called before ceiling().

        loadMeta(taskMetaA);
        loadMeta(taskMetaB);

        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);
        //Here A:4/1, B:4/2
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);

        //Since workerA has high reputation, should work on same task as B which has no reputation
        assert replicaBoxA.getTaskMeta().getTaskName().equals(replicaBoxB.getTaskMeta().getTaskName());
    }

    @Test
    public void excessReplicaTest(){
        builder.setExpectedReputation(0);
        builder.setReplicas(1);
        replicaManager = builder.create();

        final Semaphore counter = new Semaphore(0);

        replicaManager.setValidationListener(new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                counter.release();
            }
        });

        loadMeta(taskMetaA);
        byte[] result = new byte[0];

        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);

        replicaManager.replicaFinished(replicaBoxA.getReplicaID(), result);
        //Do not validate yet, wait for B
        assert counter.availablePermits() == 0;

        replicaManager.replicaFinished(replicaBoxB.getReplicaID(), result);
        //Validate here
        assert counter.availablePermits() == 1;

        ReplicaBox replicaBoxC = replicaManager.giveReplicaToWorker(workerC);
        replicaManager.replicaFinished(replicaBoxC.getReplicaID(), result);
        //excess replica -> late comer

        assert counter.availablePermits() == 2;
    }

    @Test
    public void latecomerTest(){
        builder.setExpectedReputation(0);
        builder.setReplicas(1);
        replicaManager = builder.create();

        final Semaphore validCounter = new Semaphore(0);
        final Semaphore lateCounter = new Semaphore(0);

        replicaManager.setValidationListener(new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                System.out.println("Arrived "+evt.getPropertyName());
                if("Validate".equals(evt.getPropertyName())){
                    validCounter.release();
                } else if("Late".equals(evt.getPropertyName())){
                    lateCounter.release();
                } else {
                    throw new AssertionError("Unknown: "+evt.getPropertyName());
                }
            }
        });

        loadMeta(taskMetaA);
        byte[] result = new byte[0];

        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);

        replicaManager.replicaFinished(replicaBoxA.getReplicaID(), result);
        //Validate here

        assert validCounter.availablePermits() == 1;

        //Late comer
        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);
        replicaManager.replicaFinished(replicaBoxB.getReplicaID(), result);

        assert lateCounter.availablePermits() == 1;
    }

    @Test
    public void advancedLatecomerTest(){
        builder.setExpectedReputation(0);
        builder.setReplicas(1);
        replicaManager = builder.create();

        final Semaphore validCounter = new Semaphore(0);
        final Semaphore lateCounter = new Semaphore(0);

        replicaManager.setValidationListener(new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                System.out.println("Arrived "+evt.getPropertyName());
                if("Validate".equals(evt.getPropertyName())){
                    validCounter.release();
                } else if("Late".equals(evt.getPropertyName())){
                    lateCounter.release();
                } else {
                    throw new AssertionError("Unknown: "+evt.getPropertyName());
                }
            }
        });

        loadMeta(taskMetaA);
        byte[] result = new byte[0];

        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);

        replicaManager.replicaFinished(replicaBoxA.getReplicaID(), result);
        //Can validate but chooses to wait for B
        assert validCounter.availablePermits() == 0;

        //Excess workers arrive
        ReplicaBox replicaBoxC = replicaManager.giveReplicaToWorker(workerC);
        ReplicaBox replicaBoxD = replicaManager.giveReplicaToWorker(workerD);

        replicaManager.replicaFinished(replicaBoxB.getReplicaID(), result);
        //Validate now, do not wait for excess workers!
        assert validCounter.availablePermits() == 1;

        //First late comer
        replicaManager.replicaFinished(replicaBoxC.getReplicaID(), result);
        assert lateCounter.availablePermits() == 1;

        //Second late comer
        replicaManager.replicaFinished(replicaBoxD.getReplicaID(), result);
        assert lateCounter.availablePermits() == 2;
    }

    private void promote(WorkerID workerID, int times){
        for(int i=0; i<times; ++i){
            workerReputationManager.promoteWorker(workerID);
        }
    }

    private void loadMeta(TaskMeta taskMeta){
        TestUtils.loadMeta(taskMeta, this.replicaManager);
    }

}
