package taskbuilder.communicationToClient;

/**
 * Created by joakim on 2014-04-01.
 *
 * Interface for classes that listens for validity results
 */
public interface ValidityListener {

    /**
     * Called when a validation of a workers result of a task has been successful
     * @param worker identity of the worker of the given task
     * @param taskName identification name of the task
     */
    public void validityOk(WorkerID worker, String taskName);

    /**
     * Called when a validation of a workers result of a task has found to be corrupt
     * @param worker identity of the worker of the given task
     * @param taskName identification name of the task
     */
    public void validityCorrupt(WorkerID worker, String taskName);

    /**
     * Called when a validation of a workers result of a task has been erroneous
     * @param worker identity of the worker of the given task
     * @param taskName identification name of the task
     * @param reason The reason for the error
     */
    public void validityError(WorkerID worker, String taskName, String reason);
}
