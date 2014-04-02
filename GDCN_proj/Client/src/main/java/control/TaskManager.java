package control;

import command.communicationToUI.ClientInterface;
import files.Downloader;
import files.JobUploader;
import files.TaskMeta;
import files.TaskMetaDataException;
import replica.ReplicaManager;
import taskbuilder.Task;
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

    /**
     *
     * @param taskListener Listener for the different kinds of things TaskManager can do such as Upload job or Work on task
     * @param client Client such as {@link control.PeerOwner} that can do network operations.
     */
    public TaskManager(TaskListener taskListener, ClientInterface client) {
        this.taskListener = taskListener;
        this.client = client;
    }

    //TODO use worker pool instead of new Threads
    //TODO handle Listeners more nicely...

    /**
     * Work on this task
     * @param projectName Name of working directory that contains /resources etc
     * @param taskMeta Meta information of the task
     * @param subjectListener Can be null, will be combined with the TaskManagers own listener.
     */
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

                    Task task = downloader.buildTask(TaskManager.this.taskListener);

                    Thread taskThread = new Thread(task);
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

    /**
     * Upload entire job, that is all necessary files within the directory to fulfill the TaskMetas
     * @param jobName Name of job directory
     * @param replicaManager Manager that will produce replicas of each task
     */
    public void uploadJob(final String jobName, final ReplicaManager replicaManager){
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    JobUploader jobUploader = JobUploader.create(jobName, client, taskListener, replicaManager);
                    boolean success = jobUploader.runAndAwait();

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
