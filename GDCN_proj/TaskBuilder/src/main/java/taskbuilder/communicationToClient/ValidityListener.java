package taskbuilder.communicationToClient;

/**
 * Created by joakim on 2014-04-01.
 */
public interface ValidityListener {

    public void validityOk(String worker, String taskName);

    public void validityCorrupt(String worker, String taskName);

    public void validityError(String worker, String taskName, String reason);
}
