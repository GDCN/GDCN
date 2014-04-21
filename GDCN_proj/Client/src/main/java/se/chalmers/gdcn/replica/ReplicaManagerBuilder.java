package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.control.WorkerReputationManager;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.utils.Time;

import java.util.Calendar;

/**
 * Created by Leif on 2014-04-16.
 */
public class ReplicaManagerBuilder {
    private int replicas = 3;
    private int expectedReputation = 3;

    //TODO store milliseconds instead?
    private int timeoutLengthType = Calendar.MINUTE;
    private int timeoutLengthValue = 5;

    private long timerUpdateIntervalMillis = 50000;

    private WorkerReputationManager workerReputationManager = null;
    private WorkerID myWorkerID = null;

    /**
     * @param myWorkerID this node's workerID, used in WorkerReputationManager
     */
    public ReplicaManagerBuilder(WorkerID myWorkerID) {
        this.myWorkerID = myWorkerID;
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

        return new ReplicaManager(workerReputationManager, timeoutLengthValue, timeoutLengthType, timerUpdateIntervalMillis,
                replicas, expectedReputation);
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
        this.timeoutLengthType = unit.getTypeConstant();
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
