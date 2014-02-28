package taskbuilder;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by HalfLeif on 2014-02-28.
 */
public class TaskManager{

    private final TaskListener client;
    private final TaskListener listener = new TaskListener() {
        @Override
        public void taskFinished(String taskName) {
            //TODO

            client.taskFinished(taskName);
        }

        @Override
        public void taskFailed(String taskName, String reason) {
            //TODO

            client.taskFailed(taskName, reason);
        }
    };

    private final Map<String,Thread> runningTasks = new HashMap<>();

    public TaskManager(TaskListener client) {
        this.client = client;
    }

    public void startTask(String taskName, String initData){
        Thread thread = new Thread(new Task(taskName, initData, listener));
        thread.setDaemon(true);
        runningTasks.put(taskName, thread);

        thread.start();
    }
}
