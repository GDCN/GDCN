package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.control.WorkerNodeManager;
import se.chalmers.gdcn.network.WorkerID;

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

    private WorkerNodeManager workerNodeManager = null;
    private WorkerID myWorkerID = null;

    public ReplicaManagerBuilder(WorkerID myWorkerID) {
        this.myWorkerID = myWorkerID;
    }

    /**
     * Only used for testing!
     * @param workerNodeManager WorkerNodeManager
     */
    public ReplicaManagerBuilder(WorkerNodeManager workerNodeManager) {
        this.workerNodeManager = workerNodeManager;
    }

    public ReplicaManager create(){
        if(workerNodeManager == null){
            if(myWorkerID == null){
                throw new IllegalStateException("WorkerID or WorkerNodeManager must be set before creation!");
            }
            workerNodeManager = new WorkerNodeManager(myWorkerID);
        }

        return new ReplicaManager(workerNodeManager, timeoutLengthValue, timeoutLengthType, timerUpdateIntervalMillis,
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

    public ReplicaManagerBuilder setTimeoutLength(int length, Time unit){
        this.timeoutLengthType = unit.typeConstant;
        this.timeoutLengthValue = length;
        return this;
    }

    public ReplicaManagerBuilder setTimerUpdateIntervalMillis(int length, Time unit) {
        this.timerUpdateIntervalMillis = unit.comparedToMillis*length;
        return this;
    }

    public static enum Time{
        MILLISECOND(Calendar.MILLISECOND, 1),
        SECOND(Calendar.SECOND, 1000),
        MINUTE(Calendar.MINUTE, 60*SECOND.comparedToMillis),
        HOUR(Calendar.HOUR, 3600*SECOND.comparedToMillis),
        ;
        private final int typeConstant;
        private final long comparedToMillis;

        Time(int typeConstant, long comparedToMillis) {
            this.typeConstant = typeConstant;
            this.comparedToMillis = comparedToMillis;
        }
    }
}