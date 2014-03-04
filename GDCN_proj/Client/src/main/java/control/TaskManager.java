package control;

import command.PeerOwner;
import command.communicationToUI.ClientInterface;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.Install;

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
//        Thread thread = new Thread(new Task(projectName, taskName, initData, projectName, listener));
//        thread.setDaemon(true);
//
//        runningTasks.put(taskName, thread);
//        thread.start();
    }

    //TODO use worker pool instead of new Threads
    public void startTask(String projectName, String taskName, ClientInterface networker){
        //Delegates error passing to networker (ie PeerOwner). Makes call to his listeners
        FileMaster fileMaster = new FileMaster(projectName, taskName, networker, client);

        fileMaster.await();

        Thread thread = new Thread(fileMaster.buildTask(listener));
        thread.setDaemon(true);

        runningTasks.put(taskName, thread);
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
//        PathManager pathManager = new PathManager("Primes");
        ClientInterface client = new PeerOwner();
        client.start(4098);

        TaskManager manager = new TaskManager(mainTaskListener);
        manager.startTask("PrimeTask_01.json", "Prime", client);

        semaphore.acquireUninterruptibly();
        client.stop();
    }
}
