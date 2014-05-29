package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.control.TaskManager;
import se.chalmers.gdcn.control.WorkerReputationManager;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.utils.Time;

/**
 * Created by Leif on 2014-04-16.
 */
public class ReplicaManagerBuilder {
    private int replicas = 2;
    //private int expectedReputation = 3;
    //Changed for demo
    private int expectedReputation = 1;

    //TODO store milliseconds instead?
    private Time timeoutLengthUnit = Time.MINUTE;
    private int timeoutLengthValue = 5;

    private long timerUpdateIntervalMillis = 50000;

    private WorkerReputationManager workerReputationManager = null;
    private TaskManager taskManager = null;
    private WorkerID myWorkerID = null;

    /**
     * @param myWorkerID this node's workerID, used in WorkerReputationManager
     * @param taskManager taskManager
     */
    public ReplicaManagerBuilder(WorkerID myWorkerID, TaskManager taskManager) {
        this.myWorkerID = myWorkerID;
        this.taskManager = taskManager;
    }

    /**
     * Only used for testing!
     * @param workerReputationManager WorkerReputationManager
     */
    public ReplicaManagerBuilder(WorkerReputationManager workerReputationManager) {
        this.workerReputationManager = workerReputationManager;
    }

    public ReplicaManager create(){
        if(workerReputationManager == null){
            if(myWorkerID == null){
                throw new IllegalStateException("WorkerID or WorkerReputationManager must be set before creation!");
            }
            workerReputationManager = new WorkerReputationManager(myWorkerID);
        }

        return new ReplicaManager(workerReputationManager, taskManager, timeoutLengthUnit, timerUpdateIntervalMillis,
                replicas, expectedReputation, timeoutLengthValue);
    }

    public ReplicaManagerBuilder setReplicas(int replicas) {
        this.replicas = replicas;
        return this;
    }

    public ReplicaManagerBuilder setExpectedReputation(int expectedReputation) {
        this.expectedReputation = expectedReputation;
        return this;
    }

    /**
     * Set timeout length for replica.
     * @param length time length
     * @param unit Time unit
     * @return builder object
     */
    public ReplicaManagerBuilder setTimeoutLength(int length, Time unit){
        this.timeoutLengthUnit = unit;
        this.timeoutLengthValue = length;
        return this;
    }

    /**
     * Set update time interval for timer, should be much lower than timeout interval.
     * Update is cheap so this can be quite short.
     * @param length time length
     * @param unit Time unit
     * @return builder object
     */
    public ReplicaManagerBuilder setTimerUpdateInterval(int length, Time unit) {
        this.timerUpdateIntervalMillis = unit.getComparedToMillis()*length;
        return this;
    }

}
