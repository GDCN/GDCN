package command.communicationToUI;

/**
 * Created by Leif on 2014-02-25.
 *
 * Replaces all "boolean successful" in communication between layers.
 * Not yet complete.
 */
public enum ErrorCode {

    NOT_CONNECTED("There is no listening peer on any port on this client!"),
    DISCOVER_FAILURE("Future discover failed."),
    BOOTSTRAP_FAILURE("Future bootstrap failed.");

    private final String description;

    private ErrorCode(String description) {
        this.description = description;
    }

    @Override
    public String toString() {
        return description;
    }
}
