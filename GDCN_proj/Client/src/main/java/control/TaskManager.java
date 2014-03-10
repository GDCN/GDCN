package control;

import command.communicationToUI.ClientInterface;
import files.Downloader;
import files.TaskMetaDataException;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.Install;
import taskbuilder.fileManagement.PathManager;

import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Semaphore;

/**
 * Created by HalfLeif on 2014-02-28.
 *
 * Manager of running tasks. Can run multiple tasks concurrently.
 */
public class TaskManager{

    private final Map<String,Thread> runningTasks = new HashMap<String,Thread>();

    private final TaskListener client;
    private final TaskListener listener = new TaskListener() {
        @Override
        public void taskFinished(String taskName) {
            runningTasks.remove(taskName);
            //TODO do something other than pass along signal?

            client.taskFinished(taskName);
        }

        @Override
        public void taskFailed(String taskName, String reason) {
            runningTasks.remove(taskName);
            //TODO do something other than pass along signal?

            client.taskFailed(taskName, reason);
        }
    };

    public TaskManager(TaskListener client) {
        this.client = client;
    }

    public int numberOfRunningTasks(){
        return runningTasks.size();
    }

    //TODO use worker pool instead of new Threads
    public void startTask(final String projectName, final String taskName, final ClientInterface networker){
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                //Delegates error passing to networker (ie PeerOwner). Makes call to his listeners
                try {
                    Downloader fileMaster = Downloader.create(projectName, taskName, networker, client);
                    boolean success = fileMaster.runAndAwait();

                    if(!success){
                        client.taskFailed(taskName, "Unresolved dependencies");
                        return;
                    }

                    Thread thread = new Thread(fileMaster.buildTask(listener));
                    thread.setDaemon(true);

                    runningTasks.put(taskName, thread);
                    thread.start();
                } catch (FileNotFoundException e) {
                    e.printStackTrace();
                    //TODO handle file not found
                } catch (TaskMetaDataException e) {
                    e.printStackTrace();
                    //TODO handle job owner error
                }

            }
        });
        thread.setDaemon(true);
        thread.start();
    }

    public static void main(String[] args){

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

        ClientInterface client = new PeerOwner();
        client.start(8056);

        try {
            TaskManager manager = new TaskManager(mainTaskListener);
            manager.startTask("Primes", "PrimeTask_01", client);

            System.out.println("Await task response");
            semaphore.acquireUninterruptibly();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            client.stop();
        }
    }
}
