package network;

import java.io.Serializable;

/**
 * Just a serializable message that contains a reason for the failure.
 */
class FailMessage implements Serializable {
    private final String reason;
    private final String replicaID;

    FailMessage(String reason, String replicaID) {
        this.reason = reason;
        this.replicaID = replicaID;
    }

    public String getReason() {
        return reason;
    }

    public String getReplicaID() {
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
