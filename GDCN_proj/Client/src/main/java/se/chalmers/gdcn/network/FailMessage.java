package se.chalmers.gdcn.network;

import se.chalmers.gdcn.replica.ReplicaManager2.ReplicaID;

import java.io.Serializable;

/**
 * Just a serializable message that contains a reason for the failure.
 */
class FailMessage implements Serializable {
    private final String reason;
    private final ReplicaID replicaID;

    FailMessage(String reason, ReplicaID replicaID) {
        this.reason = reason;
        this.replicaID = replicaID;
    }

    public String getReason() {
        return reason;
    }

    public ReplicaID getReplicaID() {
        return replicaID;
    }

    @Override
    public String toString() {
        return "FailMessage{" +
                "reason='" + reason + '\'' +
                ", replicaID='" + replicaID + '\'' +
                '}';
    }
}
