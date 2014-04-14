package manualTests;

import com.google.gson.Gson;
import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.control.PeerOwner;
import se.chalmers.gdcn.control.TaskManager;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.files.TaskMetaDataException;
import se.chalmers.gdcn.network.StringHolder;
import se.chalmers.gdcn.replica.ReplicaManager;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.Install;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;

import java.io.*;
import java.util.concurrent.Semaphore;

/**
 * Created by Leif on 2014-04-01.
 *
 * Not really Unit testing but manual testing...
 *
 * @deprecated Doesn't read tasks from file anymore but sends in messages
 */
public class TaskManagerManual {

    public static void main(String[] args){

        final Semaphore semaphore = new Semaphore(0);
        final TaskListener firstTaskListener = new TaskListener() {
            @Override
            public void taskFinished(String taskName) {
                System.out.println("Task finished "+taskName);
                semaphore.release();
            }

            @Override
            public void taskFailed(String taskName, String reason) {
                System.out.println("Task failed "+taskName);
                System.out.println("because of: "+reason);
                semaphore.release();
            }
        };

        final ClientInterface client = new PeerOwner();
        client.start(11789);

        try {
            TaskManager manager = new TaskManager(firstTaskListener, client);
            manager.uploadJob("Job1", new ReplicaManager(1));

            System.out.println("Await task response");
            semaphore.acquireUninterruptibly();
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.out.println("\n-- ENTER Second part! --");

        try {
            executeTaskTest(client, "PrimeTask_01");
            System.out.println("\n-- ENTER Third part: next generation tasks! --");

            executeTaskTest(client, "PrimeTask_02");
            executeTaskTest(client, "PrimeTask_03");

        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            client.stop();
        }
    }
    public static void main2(String[] args){
        ClientInterface client = new PeerOwner();
        client.start(8056);

        try {
            executeTaskTest(client, "PrimeTask_02");
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            client.stop();
        }
    }

    private static void executeTaskTest(ClientInterface client, String taskName) throws Exception{
        final Semaphore semaphore = new Semaphore(0);
        final TaskListener mainTaskListener = new TaskListener() {
            @Override
            public void taskFinished(String taskName) {
                System.out.println("Task finished "+taskName);
                semaphore.release();
            }

            @Override
            public void taskFailed(String taskName, String reason) {
                System.out.println("Task failed "+taskName);
                System.out.println("because of: "+reason);
                semaphore.release();
            }
        };

        Install.install();

        //Might want to copy "dGDCN/" to "~/.gdcn/"

        PathManager pathManager = PathManager.worker("Primes");
        pathManager.deleteBinaries();

        TaskManager manager = new TaskManager(mainTaskListener, client);
        TaskMeta taskMeta = resolveMetaFile(taskName, pathManager);
        manager.startTask("Primes", taskMeta, new StringHolder(), null);

        System.out.println("Await task response");
        semaphore.acquireUninterruptibly();
    }

    /**
     *
     * @param taskName Name of task, ie without ".json"
     * @param pathManager Pathmanager to correct directory
     * @return TaskMeta of this task (replica)
     * @throws TaskMetaDataException
     */
    public static TaskMeta resolveMetaFile(String taskName, PathManager pathManager) throws TaskMetaDataException {
        final File file = new File(pathManager.taskMetaDir() + taskName + ".json");
        if(file.exists()){
            System.out.println("Downloader: YAY file exist!");
            try {
                return readMetaFile(file);
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }
        throw new TaskMetaDataException("Error reading file: "+file.getAbsolutePath());
    }

    /**
     * Parses file for MetaData of Task
     *
     * @param file Path to meta data file
     * @return Representation of meta data content
     * @throws FileNotFoundException if file isn't found
     */
    private static TaskMeta readMetaFile(File file) throws FileNotFoundException {

        Reader reader = null;
        try {
            reader = new InputStreamReader(new BufferedInputStream(new FileInputStream(file)));

            Gson gson = new Gson();
            return gson.fromJson(reader, TaskMeta.class);

        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

}
