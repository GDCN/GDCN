package manualTests;

import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.control.PeerOwner;
import se.chalmers.gdcn.files.JobUploader;
import se.chalmers.gdcn.replica.ReplicaManager;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.Install;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;

/**
 * Created by Leif on 2014-04-01.
 */
public class UploaderManual {
    public static void main(String[] args){

//        final Semaphore semaphore = new Semaphore(0);
        final TaskListener mainTaskListener = new TaskListener() {
            @Override
            public void taskFinished(String taskName) {
                System.out.println("Task finished "+taskName);
//                semaphore.release();
            }

            @Override
            public void taskFailed(String taskName, String reason) {
                System.out.println("Task failed "+taskName);
                System.out.println("because of: "+reason);
//                semaphore.release();
            }
        };

        Install.install();

        //Might want to copy "dGDCN/" to "~/.gdcn/"

        PathManager pathManager = PathManager.jobOwner("Job1");
        ClientInterface client = new PeerOwner();
        client.start(8056);

        try {
            JobUploader jobUploader = JobUploader.create("Job1", client, mainTaskListener, new ReplicaManager(1));
            boolean success = jobUploader.runAndAwait();

            if(success){
                System.out.println("Seems to work :D");
            } else {
                System.out.println("Something whent wrong...");
            }
            //OBS doesn't need the semaphore since runs in the same thread
//            semaphore.acquire();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            client.stop();
        }
    }
}
