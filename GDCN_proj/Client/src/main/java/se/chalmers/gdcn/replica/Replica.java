package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.network.WorkerID;

import java.io.Serializable;

/**
* Created by Leif on 2014-03-31.
*/
class Replica implements Serializable{

    private final ReplicaBox replicaBox;
    private final WorkerID worker;

    /**
     * Replica is the object that job owner holds to keep track of who worked on this replica
     * and what the result of it is before it has been canonized.
     *
     * Generates a ReplicaBox that will be sent to a specific worker node.
     *
     * @param replicaBox
     * @param worker
     *
     */
    Replica(ReplicaBox replicaBox, WorkerID worker) {
        this.worker = worker;
        this.replicaBox = replicaBox;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Replica)) return false;

        Replica replica = (Replica) o;

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
}
