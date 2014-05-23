package manualTests;

import com.google.gson.Gson;
import net.tomp2p.peers.Number160;
import se.chalmers.gdcn.compare.QualityControl;
import se.chalmers.gdcn.compare.TrustQuality;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.replica.ReplicaManager.ReplicaID;
import se.chalmers.gdcn.taskbuilder.fileManagement.Install;
import se.chalmers.gdcn.utils.ByteArray;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.Map.Entry;

/**
 * Created by joakim on 4/16/14.
 */
public class QualityControlManual {

    private final static String TASK_META = "{\n" +
            "    \"taskName\":\"IncrementTask_01\",\n" +
            "    \"module\":{\"fileName\":\"Increment.hs\",\"fileLocation\":\"code\",\"sticky\":true,\"checkSum\":500},\n" +
            "    \"dependencies\":\n" +
            "    [\n" +
            "        {\"fileName\":\"0.raw\",\"fileLocation\":\"resources\",\"sticky\":false,\"checkSum\":25}\n" +
            "    ]\n" +
            "}";

    public static void main(String[] args) throws IOException{

        Gson gson = new Gson();
        TaskMeta taskMeta = gson.fromJson(TASK_META, TaskMeta.class);
        taskMeta.getModule().setDhtKey(Number160.createHash(taskMeta.getModule().getFileName()));

        List<byte[]> results = new ArrayList<>();

        // Path to the resource files
        String path = "/home/joakim/GDCN/GDCN_proj/dGDCN/jobs/TrivialJob/resources/";

        // You might want to move dGDCN to .gdcn
        // and compile Valid.hs and move to "valid" dir of the job

        Install.install();

        results.add(Files.readAllBytes(Paths.get(path + "0.raw")));
        results.add(Files.readAllBytes(Paths.get(path + "50.raw")));
        results.add(Files.readAllBytes(Paths.get(path + "100.raw")));
        results.add(Files.readAllBytes(Paths.get(path + "150.raw")));
        results.add(Files.readAllBytes(Paths.get(path + "wrong_type.raw")));

        Map<ByteArray, Set<ReplicaID>> resultMap = new HashMap<>();
        int id = 0;
        for (byte[] result : results) {
            resultMap.put(new ByteArray(result), new HashSet<ReplicaID>());
            System.out.println("Result " + id++ + " has id " + result.toString());
        }

        System.out.println("\t-------------------");

        Map<ByteArray,TrustQuality> qualityMap = QualityControl.compareQuality("TrivialJob", taskMeta, resultMap.keySet());

        for (Entry<ByteArray, TrustQuality> entry : qualityMap.entrySet()) {
            System.out.println(entry.getKey().getData().toString() + " has trust " + entry.getValue().getTrust()
                    + " and quality " + entry.getValue().getQuality());
        }
    }
}
