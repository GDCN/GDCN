package control;

import command.communicationToUI.ClientInterface;
import files.Downloader;
import files.TaskMeta;
import files.TaskMetaDataException;
import files.Uploader;
import replica.ReplicaManager;
import taskbuilder.communicationToClient.TaskListener;

import java.io.FileNotFoundException;

/**
 * Created by HalfLeif on 2014-02-28.
 *
 * Manager of running tasks. Can run multiple tasks concurrently.
 */
public class TaskManager{

    private final TaskListener taskListener;
    private final ClientInterface client;

    public TaskManager(TaskListener taskListener, ClientInterface client) {
        this.taskListener = taskListener;
        this.client = client;
    }

    //TODO use worker pool instead of new Threads
    //TODO handle Listeners more nicely...
    public void startTask(final String projectName, final TaskMeta taskMeta, final TaskListener subjectListener){
        final TaskListener combinedTaskListener = new TaskListener() {
            @Override
            public void taskFinished(String taskName) {
                if(subjectListener != null){
                    subjectListener.taskFinished(taskName);
                }
                taskListener.taskFinished(taskName);
            }

            @Override
            public void taskFailed(String taskName, String reason) {
                if(subjectListener != null){
                    subjectListener.taskFailed(taskName, reason);
                }
                taskListener.taskFailed(taskName, reason);
            }
        };

        Thread downloaderThread = new Thread(new Runnable() {
            @Override
            public void run() {
                //Delegates error passing to client (ie PeerOwner). Makes call to his listeners
                try {
                    Downloader downloader = new Downloader(taskMeta, projectName, client, combinedTaskListener);
                    boolean success = downloader.runAndAwait();

                    if(!success){
                        TaskManager.this.taskListener.taskFailed(taskMeta.getTaskName(), "Unresolved dependencies");
                        return;
                    }

                    Thread taskThread = new Thread(downloader.buildTask(TaskManager.this.taskListener));
                    taskThread.setDaemon(true);
                    taskThread.start();
                } catch (TaskMetaDataException e) {
                    e.printStackTrace();
                    //TODO handle job owner error
                }

            }
        });
        downloaderThread.setDaemon(true);
        downloaderThread.start();
    }

    //TODO use worker pool instead of new Threads
    public void uploadJob(final String jobName, final ReplicaManager replicaManager){
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Uploader uploader = Uploader.create(jobName, client, taskListener, replicaManager);
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
