package se.chalmers.gdcn.replica;

import com.google.gson.Gson;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.taskbuilder.fileManagement.Install;
import se.chalmers.gdcn.utils.ByteArray;

import java.io.IOException;
import java.util.HashSet;

/**
 * Created by joakim on 4/30/14.
 */
public class ArchiveTest {

    private final static String TASK_META = "{\n" +
            "    \"taskName\":\"Dummy\",\n" +
            "    \"module\":{\"fileName\":\"Dummy.hs\",\"fileLocation\":\"code\",\"dhtKey\":\"Dummy\",\"sticky\":true,\"checkSum\":500},\n" +
            "    \"dependencies\":\n" +
            "    [\n" +
            "        {\"fileName\":\"dummy.raw\",\"fileLocation\":\"resources\",\"dhtKey\":\"DummyRaw\",\"sticky\":false,\"checkSum\":25}\n" +
            "    ]\n" +
            "}";

    private TaskMeta taskMeta;
    private TaskData taskData;
    private Archive archive;

    private ByteArray byteArray;
    private ByteArray badByteArray;

    private CanonicalResult canonicalResult;


    @BeforeClass
    public void setupClass() {

        Install.install();

        Gson gson = new Gson();
        taskMeta = gson.fromJson(TASK_META, TaskMeta.class);

        archive = new Archive();
        taskData = new TaskData(taskMeta, "dummyJob", 1, 10);

        byte[] data = {0, 0, 0, 1};
        byteArray = new ByteArray(data);

        byte[] badData = {0, 0, 0};
        badByteArray = new ByteArray(badData);

        archive.archiveResult(taskData, byteArray, 10, new HashSet<WorkerID>());

        canonicalResult = archive.getArchivedResult(taskData.taskID());
    }

    @Test
    public void sameDataTest() {

        boolean result = false;
        try {
            result = canonicalResult.compareNewWorker(byteArray, null);
        } catch (IOException e) {
            e.printStackTrace();
        }

        assert result;
    }

    @Test
    public void differentDataTest() {

        boolean result = true;
        try {
            result = canonicalResult.compareNewWorker(badByteArray, null);
        }
        catch (IOException e) {
            e.printStackTrace();
        }

        assert !result;
    }
}
