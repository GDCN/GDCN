package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.files.TaskMeta;
import net.tomp2p.peers.Number160;
import se.chalmers.gdcn.replica.ReplicaManager.ReplicaID;

import java.io.Serializable;
import java.util.Random;

/**
* Created by Leif on 2014-04-01.
*/
public class ReplicaBox implements Serializable {

    private static final Random random = new Random();

    private final ReplicaID replicaID;
    private final TaskMeta taskMeta;
    private final Number160 resultKey;

    /**
     * ReplicaBox is the serialized object that is sent to a Worker in a message.
     *
     * @param taskMeta TaskMeta for this replica
     *
     */

    ReplicaBox(TaskMeta taskMeta) {
        this.resultKey = Number160.createHash(random.nextLong());
        this.taskMeta = taskMeta;
        this.replicaID = generateReplicaID(taskMeta);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ReplicaBox)) return false;

        ReplicaBox replicaBoxO = (ReplicaBox) o;

        if (!replicaID.equals(replicaBoxO.replicaID)) return false;
        if (!resultKey.equals(replicaBoxO.resultKey)) return false;
        if (!taskMeta.equals(replicaBoxO.taskMeta)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = replicaID.hashCode();
        result = 31 * result + taskMeta.hashCode();
        result = 31 * result + resultKey.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "ReplicaBox{" +
                "replicaID='" + replicaID + '\'' +
                ", taskMeta=" + taskMeta.toString() +
                '}';
    }

    public ReplicaID getReplicaID() {
        return replicaID;
    }

    public TaskMeta getTaskMeta() {
        return taskMeta;
    }

    public Number160 getResultKey() {
        return resultKey;
    }

    private static ReplicaID generateReplicaID(TaskMeta taskMeta){
        return new ReplicaID(taskMeta.getTaskName() + random.nextLong());
        //TODO make deterministic?
        //TODO hash replicaID or something, should not be so obvious, or should it?
    }
}
