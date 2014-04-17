package se.chalmers.gdcn.tests;

import net.tomp2p.peers.Number160;
import net.tomp2p.storage.Data;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import se.chalmers.gdcn.control.WorkerNodeManager;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.replica.ReplicaBox;
import se.chalmers.gdcn.replica.ReplicaManager;
import se.chalmers.gdcn.replica.ReplicaManager.ReplicaID;
import se.chalmers.gdcn.replica.ReplicaManagerBuilder;
import se.chalmers.gdcn.replica.ReplicaManagerBuilder.Time;
import utils.TaskHolder;
import utils.TestUtils;
import utils.WorkerHolder;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Created by Leif on 2014-04-01.
 *
 */
public class ReplicaManagerTest {

    private ReplicaManagerBuilder builder;
    private ReplicaManager replicaManager;
    private WorkerNodeManager workerNodeManager;

    private TaskMeta taskMetaA;
    private TaskMeta taskMetaB;

    private WorkerID workerA;
    private WorkerID workerB;
    private WorkerID workerC;

    private WorkerID myWorkerID;

    @BeforeClass
    public void setupClass() {
        taskMetaA = TaskHolder.getTaskA();
        taskMetaB = TaskHolder.getTaskB();

        workerA = WorkerHolder.getWorkerA();
        workerB = WorkerHolder.getWorkerB();
        workerC = WorkerHolder.getWorkerC();
        myWorkerID = WorkerHolder.getMyWorkerID();
    }

    @BeforeMethod
    public void setupMethod(){
        workerNodeManager = new WorkerNodeManager(myWorkerID);
        workerNodeManager.registerWorker(workerA);
        workerNodeManager.registerWorker(workerB);
        workerNodeManager.registerWorker(workerC);

        builder = new ReplicaManagerBuilder(workerNodeManager);
        builder.setReplicas(2);

        replicaManager = builder.create();
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

        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        TestUtils.nap(25);
        assert replicaManager.pendingReplicaIDs().size() == 0;
    }

    @Test
    public void replicaFinishTest(){
        builder.setTimeoutLength(1, Time.MILLISECOND);
        replicaManager = builder.create();

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

//    @Test
//    public void latecomerTest(){
//        loadMeta(taskMetaA);
//        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
//        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);
//        ReplicaBox replicaBoxC = replicaManager.giveReplicaToWorker(workerC);
//
//        replicaManager.replicaOutdated(replicaBoxA.getReplicaID());
//
//        replicaManager.replicaFinished(replicaBoxB.getReplicaID(), new byte[1]);
//        replicaManager.replicaFinished(replicaBoxC.getReplicaID(), new byte[1]);
//        //TODO assert validation is called
//
//        replicaManager.replicaFinished(replicaBoxA.getReplicaID(), new byte[1]);
//        //TODO assert replicaManager responds as wanted
//    }

    @Test
    public void serializedTimerTestOnReplicaManager() throws IOException, ClassNotFoundException {
        builder.setTimeoutLength(150, Time.MILLISECOND);
        builder.setTimerUpdateInterval(30, Time.MILLISECOND);
        replicaManager = builder.create();

        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        Data serialized = new Data(replicaManager);

        ReplicaManager deserialized = (ReplicaManager) serialized.getObject();

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

    private void promote(WorkerID workerID, int times){
        for(int i=0; i<times; ++i){
            workerNodeManager.promoteWorker(workerID);
        }
    }

    private void loadMeta(TaskMeta taskMeta){
        loadMeta(taskMeta, this.replicaManager);
    }

    private static void loadMeta(TaskMeta taskMeta, ReplicaManager replicaManager){
        List<TaskMeta> taskMetas = new ArrayList<>();
        taskMetas.add(taskMeta);
        replicaManager.loadTasksAndReplicate("jobName", taskMetas);
    }

}
