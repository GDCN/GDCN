package se.chalmers.gdcn.deceitful;

import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.network.AbstractDeceitfulWork;
import se.chalmers.gdcn.network.TaskPasser;
import se.chalmers.gdcn.network.TaskPasser.WorkMethod;
import se.chalmers.gdcn.replica.ReplicaBox;

/**
 * Created by HalfLeif on 2014-05-23.
 */
public class StopWork extends AbstractDeceitfulWork {
    public StopWork(ClientInterface client, TaskPasser taskPasser, Peer peer) {
        super(client, taskPasser, peer);
    }

    /**
     * Simple attempt to receive one task but not do anything with it.
     * @param jobOwner jobOwner to dupe
     */
    @Deceitful
    @Override
    public void requestWork(PeerAddress jobOwner) {
        taskPasser.requestWork(jobOwner, false, new WorkMethod() {
            @Override
            public void work(final PeerAddress jobOwner, final ReplicaBox replicaBox, final boolean autoWork) {
                final Number160 resultKey = replicaBox.getResultKey();
                final String taskName = replicaBox.getTaskMeta().getTaskName();
                System.out.println("Task " + taskName + " finished. Attempt to upload and notify job owner.");
            }
        });
    }
}
