package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.network.WorkerID;

import java.io.Serializable;

/**
* Created by Leif on 2014-03-31.
 * @deprecated
*/
class ReplicaOLD implements Serializable{

    private final ReplicaBox replicaBox;

    private WorkerID worker = null;
    private byte[] result = null;


    /**
     * ReplicaOLD is the object that job owner holds to keep track of who worked on this replica
     * and what the result of it is before it has been canonized.
     *
     * Generates a ReplicaBox that will be sent to a specific worker node.
     *
     * @param taskMeta TaskMeta for this replica
     *
     */
    ReplicaOLD(TaskMeta taskMeta) {
        replicaBox = new ReplicaBox(taskMeta);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ReplicaOLD)) return false;

        ReplicaOLD replica = (ReplicaOLD) o;

        return replicaBox.equals(replica.replicaBox);

    }

    @Override
    public int hashCode() {
        return replicaBox.hashCode();
    }

    public ReplicaBox getReplicaBox() {
        return replicaBox;
    }

    public WorkerID getWorker() {
        return worker;
    }

    public byte[] getResult() {
        return result;
    }

    public void setWorker(WorkerID worker) {
        this.worker = worker;
    }

    public void setResult(byte[] result) {
        this.result = result;
    }

}
