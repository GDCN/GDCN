package taskbuilder;

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

    public void startTask(String taskName, String moduleName, String initData){
        Thread thread = new Thread(new Task(taskName, moduleName, initData, listener));
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

        manager.startTask("TaskName_Prime_1", "Prime", "2_2000.raw");

        semaphore.acquireUninterruptibly();
    }
}
