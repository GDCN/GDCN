package network;

import java.io.Serializable;

/**
 * Just a serializable message that contains a reason for the failure.
 */
class FailMessage implements Serializable {
    private final String reason;
    private final String ID;

    FailMessage(String reason, String ID) {
        this.reason = reason;
        this.ID = ID;
    }

    public String getReason() {
        return reason;
    }

    public String getID() {
        return ID;
    }

    @Override
    public String toString() {
        return "FailMessage{" +
                "reason='" + reason + '\'' +
                ", ID='" + ID + '\'' +
                '}';
    }
}
