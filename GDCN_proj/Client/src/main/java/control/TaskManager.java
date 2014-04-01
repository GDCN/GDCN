package control;

import command.communicationToUI.ClientInterface;
import files.Downloader;
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

    public TaskManager(TaskListener taskListener) {
        this.taskListener = taskListener;
    }

    //TODO use worker pool instead of new Threads
    public void startTask(final String projectName, final String taskName, final ClientInterface networker){
        Thread downloaderThread = new Thread(new Runnable() {
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
    public void uploadJob(final String jobName, final ClientInterface networker, final ReplicaManager replicaManager){
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Uploader uploader = Uploader.create(jobName, networker, taskListener, replicaManager);
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
