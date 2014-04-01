package unitTests;

import com.google.gson.Gson;
import files.TaskMeta;
import network.WorkerID;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import replica.ReplicaManager;

import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by Leif on 2014-04-01.
 */
public class ReplicaTest {


    private ReplicaManager replicaManager;
    private TaskMeta taskMetaA;

    private KeyPairGenerator generator;
    private WorkerID workerA;
    private WorkerID workerB;
    private WorkerID workerC;

    private final Gson gson = new Gson();

    private final static String TASK_META_A = "{\n" +
            "    \"resultKey\":\"Primes1\",\n" +
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
        generator = KeyPairGenerator.getInstance("RSA");
        workerA = new WorkerID(generator.generateKeyPair().getPublic());
        workerB = new WorkerID(generator.generateKeyPair().getPublic());
        workerC = new WorkerID(generator.generateKeyPair().getPublic());
    }

    @BeforeMethod
    public void setupMethod(){
        replicaManager = new ReplicaManager(2);
    }

    @Test
    public void replicaTest(){
        //TODO implement test. Depends on how much information is sent and how much is stored.
    }

    @Test
    public void giveWorkTest(){

        assert null == replicaManager.giveReplicaToWorker(workerA);

        loadMeta(taskMetaA);
        assert null != replicaManager.giveReplicaToWorker(workerA);

        //Already has one from this task
        assert null == replicaManager.giveReplicaToWorker(workerA);

        assert null != replicaManager.giveReplicaToWorker(workerB);

        //No replicas left, only use 2 replicas in this test
        assert null == replicaManager.giveReplicaToWorker(workerC);
    }

    @Test
    public void finishReplicaTest(){
        boolean exceptionThrown = false;
        try{
            replicaManager.replicaFinished("NotARealID", "AnyResult");
        } catch (Exception e){
            exceptionThrown = true;
        }
        assert exceptionThrown;

        loadMeta(taskMetaA);
        String replicaID = replicaManager.giveReplicaToWorker(workerA);

        boolean exceptionThrown2 = false;
        try{
            replicaManager.replicaFinished(replicaID, null);
        } catch (Exception e){
            exceptionThrown2 = true;
        }
        assert exceptionThrown2;


        replicaManager.replicaFinished(replicaID, "Some result");
    }

    private void loadMeta(TaskMeta taskMeta){
        List<TaskMeta> taskMetas = new ArrayList<>();
        taskMetas.add(taskMeta);
        replicaManager.loadTasksAndReplicate(taskMetas);
    }
}
