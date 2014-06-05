<<<<<<< HEAD:GDCN_proj/Client/src/main/java/control/TaskManager.java
package control;

import command.communicationToUI.ClientInterface;
import files.Downloader;
import files.JobUploader;
import files.TaskMeta;
import files.TaskMetaDataException;
import network.StringHolder;
import replica.ReplicaManager;
import taskbuilder.Task;
import taskbuilder.communicationToClient.TaskFailureListener;
import taskbuilder.communicationToClient.TaskListener;
=======
package se.chalmers.gdcn.control;

import net.tomp2p.peers.PeerAddress;
import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.files.Downloader;
import se.chalmers.gdcn.files.JobUploader;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.files.TaskMetaDataException;
import se.chalmers.gdcn.network.StringHolder;
import se.chalmers.gdcn.replica.ReplicaManager;
import se.chalmers.gdcn.taskbuilder.Task;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskFailureListener;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
>>>>>>> dev1:GDCN_proj/Client/src/main/java/se/chalmers/gdcn/control/TaskManager.java

import java.io.FileNotFoundException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

/**
 * Created by HalfLeif on 2014-02-28.
 *
 * Manager of running tasks. Can run multiple tasks concurrently.
 */
public class TaskManager implements TaskRunner {

    private final TaskListener taskListener;
    private final ClientInterface client;

    private final ExecutorService threadPool = Executors.newFixedThreadPool(4, new ThreadFactory() {
        @Override
        public Thread newThread(Runnable r) {
            Thread thread = new Thread(r);
            thread.setDaemon(true);
            return thread;
        }
    });

    /**
     *
     * @param taskListener Listener for the different kinds of things TaskManager can do such as Upload job or work on task
     * @param client Client such as {@link se.chalmers.gdcn.control.PeerOwner} that can do network operations.
     */
    public TaskManager(TaskListener taskListener, ClientInterface client) {
        this.taskListener = taskListener;
        this.client = client;
    }

    //TODO handle Listeners more nicely...

    /**
     * Submits a runnable to run concurrently in the thread pool.
     * Is currently used for solving challenges concurrently.
     *
     * @param runnable Runnable
     */
<<<<<<< HEAD:GDCN_proj/Client/src/main/java/control/TaskManager.java
    public void submit(Runnable runnable){
        threadPool.submit(runnable);
=======
    @Override
    public void submit(Runnable runnable){
        ThreadService.submit(runnable);
    }

    @Override
    public TaskListener getTaskListener() {
        return taskListener;
>>>>>>> dev1:GDCN_proj/Client/src/main/java/se/chalmers/gdcn/control/TaskManager.java
    }

    /**
     * Work on this task
     * @param projectName Name of working directory that contains /resources etc
     * @param taskMeta Meta information of the task
     * @param resultFileNameHolder Holder that will contain the absolute path of the future result file of this task.
     * @param subjectListener Can be null, will be combined with the TaskManagers own listener.
     */
    public void startTask(final String projectName, final TaskMeta taskMeta, final StringHolder resultFileNameHolder,
<<<<<<< HEAD:GDCN_proj/Client/src/main/java/control/TaskManager.java
                          final TaskListener subjectListener){

        threadPool.submit(new Runnable() {
=======
                          final PeerAddress jobOwner, final TaskListener subjectListener){

        ThreadService.submit(new Runnable() {
>>>>>>> dev1:GDCN_proj/Client/src/main/java/se/chalmers/gdcn/control/TaskManager.java
            @Override
            public void run() {
                //Delegates error passing to client (ie PeerOwner). Makes call to his listeners
                try {
                    Downloader downloader = new Downloader(taskMeta, projectName, client, jobOwner,new TaskFailureListener() {
                        @Override
                        public void taskFailed(String taskName, String reason) {
                            if(subjectListener != null){
                                subjectListener.taskFailed(taskName, reason);
                            }
                            taskListener.taskFailed(taskName, reason);
                        }
                    });
                    boolean success = downloader.runAndAwait();
                    resultFileNameHolder.setString(downloader.futureResultFilePath());

                    if(!success){
                        TaskManager.this.taskListener.taskFailed(taskMeta.getTaskName(), "Unresolved dependencies");
                        return;
                    }

                    Task task = downloader.buildTask(new TaskListener() {
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
                    });
<<<<<<< HEAD:GDCN_proj/Client/src/main/java/control/TaskManager.java

                    final String resultPath = task.getResultFilePath();
                    resultFileNameHolder.setString(resultPath);

                    threadPool.submit(task);
=======
                    ThreadService.submit(task);
>>>>>>> dev1:GDCN_proj/Client/src/main/java/se/chalmers/gdcn/control/TaskManager.java
                } catch (TaskMetaDataException e) {
                    e.printStackTrace();
                    if(subjectListener != null) {
                        subjectListener.taskFailed(taskMeta.getTaskName(), e.getMessage());
                    }
                    taskListener.taskFailed(taskMeta.getTaskName(), e.getMessage());
                }

            }
        });
    }

    /**
     * Upload entire job, that is all necessary files within the directory to fulfill the TaskMetas
     * @param jobName Name of job directory
     * @param replicaManager Manager that will produce replicas of each task
     */
    public void uploadJob(final String jobName, final ReplicaManager replicaManager){
<<<<<<< HEAD:GDCN_proj/Client/src/main/java/control/TaskManager.java
        threadPool.submit(new Runnable() {
=======
        ThreadService.submit(new Runnable() {
>>>>>>> dev1:GDCN_proj/Client/src/main/java/se/chalmers/gdcn/control/TaskManager.java
            @Override
            public void run() {
                try {
                    JobUploader jobUploader = JobUploader.create(jobName, client, taskListener, replicaManager);
                    boolean success = jobUploader.runAndAwait();

                    if(!success){
                        taskListener.taskFailed(jobName, "Unresolved dependencies");
                    } else {
                        taskListener.taskFinished(jobName);
                        System.out.println("push done");
                    }
                } catch (FileNotFoundException e) {
                    e.printStackTrace();
                } catch (TaskMetaDataException e) {
                    e.printStackTrace();
                }
            }
        });
    }


}
