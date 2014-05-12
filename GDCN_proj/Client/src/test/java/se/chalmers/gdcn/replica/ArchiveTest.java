package se.chalmers.gdcn.replica;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.taskbuilder.fileManagement.Install;
import se.chalmers.gdcn.utils.ByteArray;
import utils.TaskHolder;

import java.io.IOException;
import java.util.HashSet;

/**
 * Created by joakim on 4/30/14.
 */
public class ArchiveTest {

    private TaskMeta taskMeta;
    private TaskData taskData;
    private Archive archive;

    private ByteArray byteArray;
    private ByteArray badByteArray;

    private CanonicalResult canonicalResult;


    @BeforeClass
    public void setupClass() {

        Install.install();
        taskMeta = TaskHolder.getTaskA();

        taskData = new TaskData(taskMeta, "dummyJob", 1, 10);

        byte[] data = {0, 0, 0, 1};
        byteArray = new ByteArray(data);

        byte[] badData = {0, 0, 0};
        badByteArray = new ByteArray(badData);
    }

    @BeforeMethod
    public void setupArchive() {
        archive = new Archive();
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
            assert false;
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
            assert false;
        }

        assert !result;
    }
}
