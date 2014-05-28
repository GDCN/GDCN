package se.chalmers.gdcn.deceitful;

import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.Operation;
import se.chalmers.gdcn.communicationToUI.OperationFinishedListener;
import se.chalmers.gdcn.control.TaskManager;
import se.chalmers.gdcn.files.FalseMeta;
import se.chalmers.gdcn.files.FileManagementUtils;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.network.AbstractDeceitfulWork;
import se.chalmers.gdcn.network.StringHolder;
import se.chalmers.gdcn.network.TaskPasser;
import se.chalmers.gdcn.network.TaskPasser.WorkMethod;
import se.chalmers.gdcn.replica.ReplicaBox;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;

import java.io.File;
import java.io.IOException;

/**
 * Created by HalfLeif on 2014-05-23.
 */
public class FalseWork extends AbstractDeceitfulWork {

    private final TaskManager taskManager;

    public FalseWork(TaskPasser taskPasser, ClientInterface client, Peer peer, TaskManager taskManager) {
        super(client, taskPasser, peer);
        this.taskManager = taskManager;
    }

    /**
     * Simple attempt returning a random result to a jobOwner for a given task.
     * Result size is hard-coded after the Module-name of the task.
     *
     * @param jobOwner jobOwner to dupe
     */
    @Deceitful
    @Override
    public void requestWork(PeerAddress jobOwner){
        taskPasser.requestWork(jobOwner, false, new WorkMethod() {
            @Override
            public void work(final PeerAddress jobOwner, final ReplicaBox replicaBox, final boolean autoWork) {
                TaskMeta originalMeta = replicaBox.getTaskMeta();
                final StringHolder stringHolder = new StringHolder();

                TaskMeta falseMeta = FalseMeta.falsify(originalMeta, "False"+originalMeta.getModule().getFileName());
                taskManager.startTask("FalseWork", falseMeta, stringHolder, jobOwner, new TaskListener() {
                    @Override
                    public void taskFinished(String taskName) {
                        final Number160 resultKey = replicaBox.getResultKey();
                        final String taskName1 = replicaBox.getTaskMeta().getTaskName();

                        client.addListener(new OperationFinishedListener(client, resultKey, CommandWord.PUT) {
                            @Override
                            protected void operationFinished(Operation operation) {
                                if (operation.isSuccess()) {
                                    System.out.println("FalseWork: Task " + taskName1 + " finished. Job owner notified if still online.");
                                    notifyJobOwner(taskPasser, jobOwner, myWorkerID, replicaBox.getReplicaID());
                                }
                            }
                        });

                        System.out.println("FalseWork: Task " + taskName + " finished. Attempt to upload and notify job owner.");
                        byte[] result = null;
                        try {
                            result = FileManagementUtils.fromFile(new File(stringHolder.getString()));
                        } catch (IOException e) {
                            e.printStackTrace();
                            taskFailed(taskName, e.getMessage());
                        }
                        if (result != null) {
                            System.out.println("\nResult holds " + result.length + " bytes.");
                            client.put(resultKey, jobOwner.getID(), new Data(result));
                        }
                    }

                    @Override
                    public void taskFailed(String taskName, String reason) {

                    }
                });
            }
        });
    }

}
