package files;

import command.communicationToUI.ClientInterface;
import control.TaskManager;
import net.tomp2p.peers.PeerAddress;

import java.util.HashMap;

/**
 * Created by Leif on 2014-03-24.
 */
public class JobOwnerAwaits {

    private ClientInterface client;
//    private Passer passer;
    private TaskManager taskManager;
    private HashMap<String, AwaitTask> awaitTaskHashMap = new HashMap<>();

    private static class AwaitTask{
        private AbstractFileMaster.TaskMeta taskMeta;
        private PeerAddress jobOwner;
        private Long ref;

        private AwaitTask(AbstractFileMaster.TaskMeta taskMeta, PeerAddress jobOwner, Long ref) {
            this.taskMeta = taskMeta;
            this.jobOwner = jobOwner;
            this.ref = ref;
        }
    }

    public void workReceived(PeerAddress jobOwner, AbstractFileMaster.TaskMeta taskMeta, Long ref){
        final String taskName = taskMeta.getTaskName();
        awaitTaskHashMap.put(taskName, new AwaitTask(taskMeta, jobOwner, ref));

        taskManager.startTask("SomeProjectName", taskName, client);
        //TODO compute tasks - now downloads meta using taskName instead of using this TaskMeta...
    }

    public void workFinishedAndUploaded(String taskName){
        AwaitTask awaitTask = awaitTaskHashMap.remove(taskName);
        if(awaitTask == null){
            //TODO report error
            System.out.println("ERROR in JobOwnerAwaits: "+taskName+" was not found in awaited tasks!");
            return;
        }
        //TODO
//        passer.sendNoReplyMessage(awaitTask.jobOwner, "CompletedYourTask", awaitTask.ref);
        System.out.println("Job owner may or not be notified about the finished task "+awaitTask.taskMeta.getTaskName());
    }
}
