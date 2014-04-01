package replica;

import files.TaskMeta;
import net.tomp2p.peers.Number160;
import network.WorkerID;

import java.io.Serializable;
import java.util.Random;

/**
* Created by Leif on 2014-03-31.
*/
class Replica implements Serializable{

    private static final Random random = new Random();

    private final String replicaID;
    private final TaskMeta taskMeta;
    private final Number160 resultKey;

    private WorkerID worker = null;
    private Object result = null;


    Replica(TaskMeta taskMeta, int index) {
        this.taskMeta = taskMeta;
        this.replicaID = generateReplicaID(taskMeta, index);
        resultKey = Number160.createHash(random.nextLong());
    }

    public String getReplicaID() {
        return replicaID;
    }

    public TaskMeta getTaskMeta() {
        return taskMeta;
    }

    public Number160 getResultKey() {
        return resultKey;
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Replica)) return false;

        Replica replica = (Replica) o;

        if (!replicaID.equals(replica.replicaID)) return false;
        if (!resultKey.equals(replica.resultKey)) return false;
        if (!taskMeta.equals(replica.taskMeta)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = replicaID.hashCode();
        result = 31 * result + taskMeta.hashCode();
        result = 31 * result + resultKey.hashCode();
        return result;
    }


    private static String generateReplicaID(TaskMeta taskMeta, int index){
        return taskMeta.getTaskName() + index;
        //TODO hash replicaID or something, should not be so obvious, or should it?
    }
}
