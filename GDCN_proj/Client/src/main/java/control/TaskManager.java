package control;

import command.communicationToUI.ClientInterface;
import files.Downloader;
import files.ResultListener;
import files.TaskMetaDataException;
import files.Uploader;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.Install;
import taskbuilder.fileManagement.PathManager;

import java.io.FileNotFoundException;
import java.util.concurrent.Semaphore;

/**
 * Created by HalfLeif on 2014-02-28.
 *
 * Manager of running tasks. Can run multiple tasks concurrently.
 */
public class TaskManager{

//    private final Map<String,Thread> runningTasks = new HashMap<>();

    private final TaskListener taskListener;
//    private final TaskListener localListener = new TaskListener() {
//        @Override
//        public void taskFinished(String taskName) {
//            runningTasks.remove(taskName);
//            //TODO do something other than pass along signal?
//
//            taskListener.taskFinished(taskName);
//        }
//
//        @Override
//        public void taskFailed(String taskName, String reason) {
//            runningTasks.remove(taskName);
//            //TODO do something other than pass along signal?
//
//            taskListener.taskFailed(taskName, reason);
//        }
//    };

    public TaskManager(TaskListener taskListener) {
        this.taskListener = taskListener;
    }

//    public int numberOfRunningTasks(){
//        return runningTasks.size();
//    }

    //TODO use worker pool instead of new Threads

    public void startTask(final String projectName, final String taskName, final ClientInterface networker, final ResultListener resultListener){
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                //Delegates error passing to networker (ie PeerOwner). Makes call to his listeners
                try {
                    Downloader downloader = Downloader.create(projectName, taskName, networker, taskListener);
                    boolean success = downloader.runAndAwait();

                    if(!success){
                        taskListener.taskFailed(taskName, "Unresolved dependencies");
                        return;
                    }

                    Thread taskThread = new Thread(downloader.buildTask(taskListener));
                    taskThread.setDaemon(true);

//                    runningTasks.put(taskName, thread);
                    taskThread.start();
                } catch (TaskMetaDataException e) {
                    e.printStackTrace();
                    //TODO handle job owner error
                }

            }
        });
        thread.setDaemon(true);
        thread.start();
    }

    //TODO use worker pool instead of new Threads
    public void uploadJob(final String jobName, final ClientInterface networker){
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Uploader uploader = Uploader.create(jobName, networker, taskListener);
                    boolean success = uploader.runAndAwait();

                    if(!success){
                        taskListener.taskFailed(jobName, "Unresolved dependencies");
                    } else {
                        taskListener.taskFinished(jobName);
                    }

                } catch (FileNotFoundException e) {
                    e.printStackTrace();
                } catch (TaskMetaDataException e) {
                    e.printStackTrace();
                }
            }
        });
        thread.setDaemon(true);
        thread.start();
    }


}
