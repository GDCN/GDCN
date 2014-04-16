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
import se.chalmers.gdcn.replica.ReplicaManagerBuilder.Time;
import utils.TestUtils;

import java.io.IOException;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
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

    private final Gson gson = new Gson();

    private final static String TASK_META_A = "{\n" +
            "    \"taskName\":\"PrimeTask_01\",\n" +
            "    \"module\":{\"fileName\":\"Prime.hs\",\"fileLocation\":\"code\",\"dhtKey\":\"Primes_algorithms\",\"sticky\":true,\"checkSum\":500},\n" +
            "    \"dependencies\":\n" +
            "    [\n" +
            "        {\"fileName\":\"2_10000.raw\",\"fileLocation\":\"resources\",\"dhtKey\":\"Primes_2_2000\",\"sticky\":false,\"checkSum\":25}\n" +
            "    ]\n" +
            "}";

    private final static String TASK_META_B = "{\n" +
            "    \"taskName\":\"PrimeTask_02\",\n" +
            "    \"module\":{\"fileName\":\"Prime.hs\",\"fileLocation\":\"code\",\"dhtKey\":\"Primes_algorithms\",\"sticky\":true,\"checkSum\":500},\n" +
            "    \"dependencies\":\n" +
            "    [\n" +
            "        {\"fileName\":\"2_10000.raw\",\"fileLocation\":\"resources\",\"dhtKey\":\"Primes_2_2000\",\"sticky\":false,\"checkSum\":25}\n" +
            "    ]\n" +
            "}";

    @BeforeClass
    public void setupClass() throws NoSuchAlgorithmException {
        taskMetaA = gson.fromJson(TASK_META_A, TaskMeta.class);
        taskMetaB = gson.fromJson(TASK_META_B, TaskMeta.class);
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

        assert null != replicaManager.giveReplicaToWorker(workerA);
        assert null != replicaManager.giveReplicaToWorker(workerA);

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

    private void loadMeta(TaskMeta taskMeta){
        loadMeta(taskMeta, this.replicaManager);
    }

    private static void loadMeta(TaskMeta taskMeta, ReplicaManager replicaManager){
        List<TaskMeta> taskMetas = new ArrayList<>();
        taskMetas.add(taskMeta);
        replicaManager.loadTasksAndReplicate("jobName", taskMetas);
    }

}
