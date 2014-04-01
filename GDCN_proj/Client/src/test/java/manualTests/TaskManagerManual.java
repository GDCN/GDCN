package manualTests;

import command.communicationToUI.ClientInterface;
import control.PeerOwner;
import control.TaskManager;
import files.ResultListener;
import replica.ReplicaManager;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.Install;
import taskbuilder.fileManagement.PathManager;

import java.util.concurrent.Semaphore;

/**
 * Created by Leif on 2014-04-01.
 *
 * Not really Unit testing but manual testing...
 */
public class TaskManagerManual {

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

        final ClientInterface client = new PeerOwner();
        client.start(11789);

        try {
            TaskManager manager = new TaskManager(firstTaskListener);
            manager.uploadJob("Job1", client, new ReplicaManager(1));

            System.out.println("Await task response");
            semaphore.acquireUninterruptibly();
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.out.println("\n-- ENTER Second part! --");

        ResultListener ignore = new ResultListener() {
            @Override
            public void taskCompleted(byte[] results) {
                //ignore
            }
        };
        try {
            executeTaskTest(client, "PrimeTask_01", ignore);
            System.out.println("\n-- ENTER Third part: next generation tasks! --");

            executeTaskTest(client, "PrimeTask_02", ignore);
            executeTaskTest(client, "PrimeTask_03", ignore);

        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            client.stop();
        }
    }
    public static void main2(String[] args){
        ClientInterface client = new PeerOwner();
        client.start(8056);

        try {
            executeTaskTest(client, "PrimeTask_02", new ResultListener() {
                @Override
                public void taskCompleted(byte[] results) {
                    //ignore
                }
            });
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            client.stop();
        }
    }

    private static void executeTaskTest(ClientInterface client, String taskName, final ResultListener resultListener) throws Exception{
        final Semaphore semaphore = new Semaphore(0);
        final TaskListener mainTaskListener = new TaskListener() {
            @Override
            public void taskFinished(String taskName) {
                System.out.println("Task finished "+taskName);
                semaphore.release();
                resultListener.taskCompleted(null);
            }

            @Override
            public void taskFailed(String taskName, String reason) {
                System.out.println("Task failed "+taskName);
                System.out.println("because of: "+reason);
                semaphore.release();
                resultListener.taskCompleted(null);
            }
        };

        Install.install();

        //Might want to copy "dGDCN/" to "~/.gdcn/"

        PathManager pathManager = PathManager.worker("Primes");
        pathManager.deleteBinaries();

        TaskManager manager = new TaskManager(mainTaskListener);
        manager.startTask("Primes", taskName, client);

        System.out.println("Await task response");
        semaphore.acquireUninterruptibly();
    }
}
