package se.chalmers.gdcn.network;

import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import se.chalmers.gdcn.Deceitful;
import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.Operation;
import se.chalmers.gdcn.communicationToUI.OperationFinishedListener;
import se.chalmers.gdcn.network.TaskPasser.WorkMethod;
import se.chalmers.gdcn.replica.ReplicaBox;

import java.util.Random;

/**
 * Created by HalfLeif on 2014-05-23.
 */
@Deceitful
public class FalseWork{

    private final TaskPasser taskPasser;
    private final ClientInterface client;
    private final WorkerID myWorkerID;

    private final Random random = new Random();

    public FalseWork(TaskPasser taskPasser, ClientInterface client, Peer peer) {
        this.taskPasser = taskPasser;
        this.client = client;
        this.myWorkerID = new WorkerID(peer.getPeerBean().getKeyPair().getPublic());
    }

    /**
     * Simple attempt returning a random result to a jobOwner for a given task.
     * Result size is hard-coded after the Module-name of the task.
     *
     * @param jobOwner jobOwner to dupe
     */
    @Deceitful
    public void requestWork(PeerAddress jobOwner){
        taskPasser.requestWork(jobOwner, false, new WorkMethod() {
            @Override
            public void work(final PeerAddress jobOwner, final ReplicaBox replicaBox, final boolean autoWork) {
                final Number160 resultKey = replicaBox.getResultKey();
                final String taskName = replicaBox.getTaskMeta().getTaskName();
                System.out.println("Task " + taskName + " finished. Attempt to upload and notify job owner.");

                client.addListener(new OperationFinishedListener(client, resultKey, CommandWord.PUT) {
                    @Override
                    protected void operationFinished(Operation operation) {
                        if(operation.isSuccess()){
                            System.out.println("Task "+taskName+" finished. Job owner notified if still online.");
                            taskPasser.sendNoReplyMessage(jobOwner, new TaskMessage(TaskMessageType.RESULT_UPLOADED, myWorkerID,
                                    replicaBox.getReplicaID()));
                        }
                    }
                });

                byte[] result;
                String moduleName = replicaBox.getTaskMeta().getModule().getFileName();

                if("Langermann.hs".equals(moduleName)){
                    result = new byte[58];
                } else if("Prime.hs".equals(moduleName)) {
                    result = new byte[6153];
                } else {
                    System.out.println("FalseWork: Unknown module - "+moduleName);
                    result = new byte[100];
                }
                random.nextBytes(result);
                System.out.println("Returning "+result.length+" random bytes.");

                client.put(resultKey, jobOwner.getID(), new Data(result));
            }
        });
    }
}
