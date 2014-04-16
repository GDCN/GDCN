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

    public void setReplicas(int replicas) {
        this.replicas = replicas;
    }

    public void setExpectedReputation(int expectedReputation) {
        this.expectedReputation = expectedReputation;
    }

    public void setTimeoutLength(int length, Time unit){
        this.timeoutLengthType = unit.typeConstant;
        this.timeoutLengthValue = length;
    }

    public void setTimerUpdateIntervalMillis(int length, Time unit) {
        this.timerUpdateIntervalMillis = unit.comparedToMillis*length;
    }

    public void setWorkerNodeManager(WorkerNodeManager workerNodeManager) {
        this.workerNodeManager = workerNodeManager;
    }

    public void setMyWorkerID(WorkerID myWorkerID) {
        this.myWorkerID = myWorkerID;
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
