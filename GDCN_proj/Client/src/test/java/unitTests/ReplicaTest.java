package unitTests;

import com.google.gson.Gson;
import files.TaskMeta;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import replica.ReplicaManager;

/**
 * Created by Leif on 2014-04-01.
 */
public class ReplicaTest {

    private ReplicaManager replicaManager;
    private TaskMeta taskMetaA;

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
    public void setupClass(){
        taskMetaA = gson.fromJson(TASK_META_A, TaskMeta.class);
    }

    @BeforeMethod
    public void setupMethod(){
        replicaManager = new ReplicaManager(3);
    }

    @Test
    public void replicaTest(){
        //TODO implement test. Depends on how much information is sent and how much is stored.
    }
}
