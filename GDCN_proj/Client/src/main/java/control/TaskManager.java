package control;

import command.communicationToUI.ClientInterface;
import taskbuilder.Task;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.Install;
import taskbuilder.fileManagement.PathManager;

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
            //TODO more stuff?

            client.taskFinished(taskName);
        }

        @Override
        public void taskFailed(String taskName, String reason) {
            runningTasks.remove(taskName);
            //TODO more stuff?

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
    @Deprecated
    public void startTask(String projectName, String taskName, String initData){
        Thread thread = new Thread(new Task(projectName, taskName, initData, projectName, listener));
        thread.setDaemon(true);

        runningTasks.put(taskName, thread);
        thread.start();
    }

    public void startTask(String projectName, String taskName, ClientInterface networker){
        //Delegates error passing to networjker (ie PeerOwner). Makes call to his listeners
        FileMaster fileMaster = new FileMaster(projectName, taskName, networker, client);

        fileMaster.await();

        //TODO real initdata
        Thread thread = new Thread(new Task(projectName, taskName, "initData.file", projectName, listener));
        thread.setDaemon(true);

        runningTasks.put(taskName, thread);
        thread.start();
    }

    public static void main(String[] args){

        final Semaphore semaphore = new Semaphore(0);

        TaskManager manager = new TaskManager(new TaskListener() {
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
        });

        Install.install();
        PathManager pathManager = new PathManager("Primes");

        manager.startTask("PrimeTask_01.json", "Prime", "2_2000.raw");

        semaphore.acquireUninterruptibly();
    }
}
