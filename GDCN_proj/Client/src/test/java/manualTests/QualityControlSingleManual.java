package manualTests;

import com.google.gson.Gson;
import se.chalmers.gdcn.compare.QualityControl;
import se.chalmers.gdcn.compare.TrustQuality;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.taskbuilder.fileManagement.Install;
import se.chalmers.gdcn.utils.ByteArray;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Created by joakim on 4/16/14.
 */
public class QualityControlSingleManual {

    private final static String TASK_META = "{\n" +
            "    \"taskName\":\"IncrementTask_01\",\n" +
            "    \"module\":{\"fileName\":\"Increment.hs\",\"fileLocation\":\"code\",\"dhtKey\":\"Increment_algorithms\",\"sticky\":true,\"checkSum\":500},\n" +
            "    \"dependencies\":\n" +
            "    [\n" +
            "        {\"fileName\":\"0.raw\",\"fileLocation\":\"resources\",\"dhtKey\":\"Increment_0\",\"sticky\":false,\"checkSum\":25}\n" +
            "    ]\n" +
            "}";

    public static void main(String[] args) throws IOException{

        Gson gson = new Gson();
        TaskMeta taskMeta = gson.fromJson(TASK_META, TaskMeta.class);

        byte[] result;

        // Path to the resource files
        String path = "/home/joakim/GDCN/GDCN_proj/dGDCN/jobs/TrivialJob/resources/";

        // You might want to move dGDCN to .gdcn
        // and compile Valid.hs and move to "valid" dir of the job

        Install.install();

        result = Files.readAllBytes(Paths.get(path + "0.raw"));
        //result = Files.readAllBytes(Paths.get(path + "50.raw"));
        //result = Files.readAllBytes(Paths.get(path + "100.raw"));
        //result = Files.readAllBytes(Paths.get(path + "150.raw"));
        //result = Files.readAllBytes(Paths.get(path + "wrong_type.raw"));

        TrustQuality quality = QualityControl.singleQualityTest("TrivialJob", taskMeta, new ByteArray(result));

        System.out.println("Trust level is " + quality.getTrust() + " and quality is " + quality.getQuality());
    }
}
