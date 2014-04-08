package replica;

import files.TaskMeta;
import network.WorkerID;

import java.io.Serializable;

/**
* Created by Leif on 2014-03-31.
*/
class Replica implements Serializable{

    private final ReplicaBox replicaBox;

    private WorkerID worker = null;
    private byte[] result = null;


    /**
     * Replica is the object that job owner holds to keep track of who worked on this replica
     * and what the result of it is before it has been canonized.
     *
     * Generates a ReplicaBox that will be sent to a specific worker node.
     *
     * @param taskMeta TaskMeta for this replica
     * @param index Index used to generate replicaID
     */
    Replica(TaskMeta taskMeta, int index) {
        replicaBox = new ReplicaBox(taskMeta, index);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Replica)) return false;

        Replica replica = (Replica) o;

        if (!replicaBox.equals(replica.replicaBox)) return false;

        return true;
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
