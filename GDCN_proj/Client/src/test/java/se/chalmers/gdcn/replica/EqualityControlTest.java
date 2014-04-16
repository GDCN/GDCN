package se.chalmers.gdcn.replica;

import com.google.gson.Gson;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.utils.ByteArray;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Created by joakim on 4/3/14.
 */
public class EqualityControlTest {

    private TaskMeta taskMeta;

    private ReplicaOLD replicaA;
    private ReplicaOLD replicaB;
    private ReplicaOLD replicaC;
    private ReplicaOLD replicaD;

    private WorkerID workerA;
    private WorkerID workerB;
    private WorkerID workerC;
    private WorkerID workerD;

    private final Gson gson = new Gson();

    private final static String TASK_META = "{\n" +
            "    \"taskName\":\"PrimeTask_01\",\n" +
            "    \"module\":{\"fileName\":\"Prime.hs\",\"fileLocation\":\"code\",\"dhtKey\":\"Primes_algorithms\",\"sticky\":true,\"checkSum\":500},\n" +
            "    \"dependencies\":\n" +
            "    [\n" +
            "        {\"fileName\":\"2_10000.raw\",\"fileLocation\":\"resources\",\"dhtKey\":\"Primes_2_2000\",\"sticky\":false,\"checkSum\":25}\n" +
            "    ]\n" +
            "}";

    @BeforeClass
    public void setupClass() {
        //Need a taskMeta dummy for replica creation
        taskMeta = gson.fromJson(TASK_META, TaskMeta.class);

        replicaA = new ReplicaOLD(taskMeta);
        byte[] resultA = {1, 2, 3, 4};
        workerA = new WorkerID(null);
        replicaA.setResult(resultA);
        replicaA.setWorker(workerA);

        replicaB = new ReplicaOLD(taskMeta);
        workerB = new WorkerID(null);
        replicaB.setResult(resultA);
        replicaB.setWorker(workerB);

        replicaC = new ReplicaOLD(taskMeta);
        byte[] resultC = {1, 2, 3, 4};
        workerC = new WorkerID(null);
        replicaC.setResult(resultC);
        replicaC.setWorker(workerC);

        replicaD = new ReplicaOLD(taskMeta);
        byte[] resultD = {1, 2, 3, 0};
        workerD = new WorkerID(null);
        replicaD.setResult(resultD);
        replicaD.setWorker(workerD);
    }

    @Test
    public void compareEqual() {
        List<ReplicaOLD> list = new ArrayList<>();

        list.add(replicaA);
        list.add(replicaB);
        list.add(replicaC);

        Map<ByteArray, List<WorkerID>> resultMap = EqualityControl.compareData(list);
        List<WorkerID> workers = resultMap.get(new ByteArray(replicaA.getResult()));

        assert workers != null;
        assert resultMap.size() == 1;
        assert workers.get(0) == workerA;
        assert workers.get(1) == workerB;
        assert workers.get(2) == workerC;
    }

    @Test
    public void compareInequal() {
        List<ReplicaOLD> list = new ArrayList<>();

        list.add(replicaA);
        list.add(replicaD);

        Map<ByteArray, List<WorkerID>> resultMap = EqualityControl.compareData(list);
        List<WorkerID> workers1 = resultMap.get(new ByteArray(replicaA.getResult()));
        List<WorkerID> workers2 = resultMap.get(new ByteArray(replicaD.getResult()));

        assert workers1 != null;
        assert workers2 != null;
        assert resultMap.size() == 2;
        assert workers1.get(0) == workerA;
        assert workers2.get(0) == workerD;
    }

    @Test
    public void compareMixed() {
        List<ReplicaOLD> list = new ArrayList<>();

        list.add(replicaA);
        list.add(replicaC);
        list.add(replicaD);

        Map<ByteArray, List<WorkerID>> resultMap = EqualityControl.compareData(list);
        List<WorkerID> workers1 = resultMap.get(new ByteArray(replicaA.getResult()));
        List<WorkerID> workers2 = resultMap.get(new ByteArray(replicaD.getResult()));

        assert workers1 != null;
        assert workers2 != null;
        assert resultMap.size() == 2;
        assert workers1.get(0) == workerA;
        assert workers1.get(1) == workerC;
        assert workers2.get(0) == workerD;
    }
}
