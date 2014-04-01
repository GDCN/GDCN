package replica;

import files.TaskMeta;
import network.WorkerID;

/**
* Created by Leif on 2014-03-31.
*/
class Replica{

    private final ReplicaBox replicaBox;

    private WorkerID worker = null;
    private Object result = null;


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

    public Object getResult() {
        return result;
    }

    public void setWorker(WorkerID worker) {
        this.worker = worker;
    }

    public void setResult(Object result) {
        this.result = result;
    }

}
