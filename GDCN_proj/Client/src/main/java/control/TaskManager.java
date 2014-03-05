package control;

import command.PeerOwner;
import command.communicationToUI.ClientInterface;
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

    /**
     * Deleted function...
     * @param projectName
     * @param taskName
     * @param initData
     */
    @Deprecated
    public void startTask(String projectName, String taskName, String initData){
        //TODO remove this method and replace uses
        throw new UnsupportedOperationException("This method is deprecated!");
//        Thread thread = new Thread(new Task(projectName, taskName, initData, projectName, listener));
//        thread.setDaemon(true);
//
//        runningTasks.put(taskName, thread);
//        thread.start();
    }

    //TODO use worker pool instead of new Threads
    public void startTask(String projectName, String taskName, ClientInterface networker){
        Thread thread = new Thread(createTask(projectName, taskName, networker));
        thread.setDaemon(true);
        thread.start();
    }

    private Runnable createTask(final String projectName, final String taskName, final ClientInterface networker){
        return new Runnable() {
            @Override
            public void run() {
                //Delegates error passing to networker (ie PeerOwner). Makes call to his listeners
                FileMaster fileMaster = null;
                try {
                    fileMaster = new FileMaster(projectName, taskName, networker, client);
                    fileMaster.runAndAwait();

                    Thread thread = new Thread(fileMaster.buildTask(listener));
                    thread.setDaemon(true);

                    runningTasks.put(taskName, thread);
                    thread.start();
                } catch (FileNotFoundException e) {
                    e.printStackTrace();
                    //TODO handle file not found
                }

            }
        };
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

        PathManager pathManager = new PathManager("Primes");
        pathManager.deleteBinaries();

        ClientInterface client = new PeerOwner();
        client.start(8056);

        try {
            TaskManager manager = new TaskManager(mainTaskListener);
            manager.startTask("Primes", "PrimeTask_01_TEST", client);

            System.out.println("Await task response");
            semaphore.acquireUninterruptibly();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            client.stop();
        }
    }
}
