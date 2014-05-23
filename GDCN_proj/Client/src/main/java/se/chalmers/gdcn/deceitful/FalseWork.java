package se.chalmers.gdcn.deceitful;

import net.tomp2p.peers.PeerAddress;
import se.chalmers.gdcn.network.TaskPasser;
import se.chalmers.gdcn.network.TaskPasser.WorkMethod;
import se.chalmers.gdcn.replica.ReplicaBox;

/**
 * Created by HalfLeif on 2014-05-23.
 */
public class FalseWork{

    private final TaskPasser taskPasser;

    public FalseWork(TaskPasser taskPasser) {
        this.taskPasser = taskPasser;
    }

    public void requestWork(PeerAddress jobOwner){
        taskPasser.requestWork(jobOwner, false, new WorkMethod() {
            @Override
            public void work(PeerAddress jobOwner, ReplicaBox replicaBox, boolean autoWork) {

            }
        });
    }
}
