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

    /**
     * @deprecated TODO clean up
     * @param projectName
     * @param taskName
     * @param networker
     */
    public void startTask(final String projectName, final String taskName, final ClientInterface networker){
        startTask(projectName, taskName, networker, new ResultListener() {
            @Override
            public void taskCompleted(byte[] results) {
                //TODO remove this overloaded method and start cleaning
            }
        });
    }

    //TODO use worker pool instead of new Threads

    public void startTask(final String projectName, final String taskName, final ClientInterface networker, final ResultListener someListener){
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

                    Thread thread = new Thread(downloader.buildTask(taskListener));
                    thread.setDaemon(true);

//                    runningTasks.put(taskName, thread);
                    thread.start();
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

    public static void main(String[] args){

        final Semaphore semaphore = new Semaphore(0);
        final TaskListener firstTaskListener = new TaskListener() {
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

        ClientInterface client = new PeerOwner();
        client.start(11789);

        try {
            TaskManager manager = new TaskManager(firstTaskListener);
            manager.uploadJob("Job1", client);

            System.out.println("Await task response");
            semaphore.acquireUninterruptibly();
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.out.println("\n-- ENTER Second part! --");
        executeTaskTest(client);
    } 
    public static void main2(String[] args){
        ClientInterface client = new PeerOwner();
        client.start(8056);

        executeTaskTest(client);
    }

    private static void executeTaskTest(ClientInterface client){
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

        PathManager pathManager = PathManager.worker("Primes");
        pathManager.deleteBinaries();

        try {
            TaskManager manager = new TaskManager(mainTaskListener);
            manager.startTask("Primes", "PrimeTask_01", client);

            System.out.println("Await task response");
            semaphore.acquireUninterruptibly();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            client.stop();
        }
    }
}
