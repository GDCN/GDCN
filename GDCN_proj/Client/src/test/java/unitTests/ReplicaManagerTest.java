package unitTests;

import com.google.gson.Gson;
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

import java.io.IOException;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by Leif on 2014-04-01.
 *
 */
public class ReplicaManagerTest {

    private ReplicaManagerBuilder builder;
    private ReplicaManager replicaManager;
    private WorkerNodeManager workerNodeManager;
    private TaskMeta taskMetaA;

    private WorkerID workerA;
    private WorkerID workerB;
    private WorkerID workerC;

    private WorkerID myWorkerID;

    private final Gson gson = new Gson();

    private final static String TASK_META_A = "{\n" +
            "    \"taskName\":\"PrimeTask_01\",\n" +
            "    \"module\":{\"fileName\":\"Prime.hs\",\"fileLocation\":\"code\",\"dhtKey\":\"Primes_algorithms\",\"sticky\":true,\"checkSum\":500},\n" +
            "    \"dependencies\":\n" +
            "    [\n" +
            "        {\"fileName\":\"2_10000.raw\",\"fileLocation\":\"resources\",\"dhtKey\":\"Primes_2_2000\",\"sticky\":false,\"checkSum\":25}\n" +
            "    ]\n" +
            "}";

    @BeforeClass
    public void setupClass() throws NoSuchAlgorithmException {
        taskMetaA = gson.fromJson(TASK_META_A, TaskMeta.class);
        KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA");

        workerA = new WorkerID(generator.generateKeyPair().getPublic());
        workerB = new WorkerID(generator.generateKeyPair().getPublic());
        workerC = new WorkerID(generator.generateKeyPair().getPublic());
        myWorkerID = new WorkerID(generator.generateKeyPair().getPublic());


    }

    @BeforeMethod
    public void setupMethod(){
        workerNodeManager = new WorkerNodeManager(myWorkerID);

        builder = new ReplicaManagerBuilder(workerNodeManager);
        builder.setReplicas(2);

        replicaManager = builder.create();
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
    public void replicaOutdatedTest(){
        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);

//        assert null == replicaManager.giveReplicaToWorker(workerC);

        //TODO something is not right here!

        replicaManager.replicaOutdated(replicaBoxA.getReplicaID());
        ReplicaBox replicaBoxC = replicaManager.giveReplicaToWorker(workerC);

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
    }
/*
    @Test
    public void outdateFinishTest(){
        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);

        replicaManager.replicaFinished(replicaBoxA.getReplicaID(), new byte[1]);
        replicaManager.replicaOutdated(replicaBoxA.getReplicaID());
        //No exception
    }

    @Test
    public void expectedReturnTest(){
//        ReplicaManager replicaManagerE = new ReplicaManager(myWorkerID, 2, 1);
//        loadMeta(taskMetaA, replicaManagerE);
//
//        ReplicaBox replicaBoxA = replicaManagerE.giveReplicaToWorker(workerA);
//        replicaManagerE.replicaFinished(replicaBoxA.getReplicaID(), new byte[1]);
        //TODO assert validation is called
    }
//
//    @Test
//    public void constructorTest(){
//        boolean exceptionThrown = false;
//        try {
//            new ReplicaManager(0,0);
//        } catch (Exception e) {
//            exceptionThrown = true;
//        } finally {
//            assert exceptionThrown;
//        }
//
//        exceptionThrown = false;
//        try {
//            new ReplicaManager(3,4);
//        } catch (Exception e) {
//            exceptionThrown = true;
//        } finally {
//            assert exceptionThrown;
//        }
//    }

    @Test
    public void latecomerTest(){
        loadMeta(taskMetaA);
        ReplicaBox replicaBoxA = replicaManager.giveReplicaToWorker(workerA);
        ReplicaBox replicaBoxB = replicaManager.giveReplicaToWorker(workerB);
        assert null == replicaManager.giveReplicaToWorker(workerC);

        replicaManager.replicaOutdated(replicaBoxA.getReplicaID());
        ReplicaBox replicaBoxC = replicaManager.giveReplicaToWorker(workerC);

        replicaManager.replicaFinished(replicaBoxB.getReplicaID(), new byte[1]);
        replicaManager.replicaFinished(replicaBoxC.getReplicaID(), new byte[1]);
        //TODO assert validation is called

        replicaManager.replicaFinished(replicaBoxA.getReplicaID(), new byte[1]);
        //TODO assert replicaManager responds as wanted
    }

    @Test
    public void integrationReplicaTimerTest() throws IOException, ClassNotFoundException {
//        ReplicaManager replicaManager2 = new ReplicaManager(2, 2, 300, Calendar.MILLISECOND, 50L);
//        loadMeta(this.taskMetaA, replicaManager2);
//
//        ReplicaBox replicaBoxA = replicaManager2.giveReplicaToWorker(workerA);
//        Data serialized = new Data(replicaManager2.clone());
//
//        ReplicaManager deserialized = (ReplicaManager) serialized.getObject();
//        assert deserialized.isWorkerAssignedReplica(workerA, replicaBoxA.getReplicaID());
//        assert null != deserialized.giveReplicaToWorker(workerB);
//        assert null == deserialized.giveReplicaToWorker(workerC);
//
//        deserialized.resumeTimer();
//        try {
//            Thread.sleep(350);
//        } catch (InterruptedException e) {
//            e.printStackTrace();
//        }
//        assert null != deserialized.giveReplicaToWorker(workerC);
    }
*/
    private void loadMeta(TaskMeta taskMeta){
        loadMeta(taskMeta, this.replicaManager);
    }

    private static void loadMeta(TaskMeta taskMeta, ReplicaManager replicaManager){
        List<TaskMeta> taskMetas = new ArrayList<>();
        taskMetas.add(taskMeta);
        replicaManager.loadTasksAndReplicate("jobName", taskMetas);
    }

}
