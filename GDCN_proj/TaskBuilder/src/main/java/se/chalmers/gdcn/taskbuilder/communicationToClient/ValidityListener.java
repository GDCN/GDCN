package se.chalmers.gdcn.taskbuilder.communicationToClient;

/**
 * Created by joakim on 2014-04-01.
 *
 * Interface for classes that listens for validity results
 */
public interface ValidityListener {

    /**
     * Called when a validation of a workers result of a task has been successful
     * @param quality
     */
    public void validityOk(int quality);

    /**
     * Called when a validation of a workers result of a task has found to be corrupt
     */
    public void validityCorrupt();

    /**
     * Called when a validation of a workers result of a task has been erroneous
     * @param reason The reason for the error
     */
    public void validityError(String reason);
}
