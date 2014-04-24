package se.chalmers.gdcn.control;

import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.utils.SerializableTimer;
import se.chalmers.gdcn.utils.Time;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by Leif on 2014-04-21.
 */
public class WorkerTimeoutManager implements Serializable{

    private final Set<WorkerID> activeWorkers = new HashSet<>();
    private final Set<WorkerID> passiveWorkers = new HashSet<>();

    private final WorkTimer workTimer;

    private final Time unit;
    private final int timeValue;

    public WorkerTimeoutManager(long updateTime, Time unit, int timeValue) {
        this.unit = unit;
        this.timeValue = timeValue;
        workTimer = new WorkTimer(updateTime);
    }

    public void activate(WorkerID workerID){
        if(passiveWorkers.remove(workerID)){
            activeWorkers.add(workerID);
        }
        workTimer.reset(workerID, Time.futureDate(unit, timeValue));
    }

    public Set<WorkerID> getActiveWorkers(){
        return new HashSet<>(activeWorkers);
    }

    public Runnable timerRunner(){
        return workTimer.createUpdater();
    }

//    public void resumeTimer(){
//        SerializableTimer.resume(workTimer);
//    }

    private class WorkTimer extends SerializableTimer<WorkerID>{

        /**
         * @param updateTime Number of Milliseconds between check queue
         */
        public WorkTimer(long updateTime) {
            super(updateTime);
        }

        @Override
        protected void handleTimeout(WorkerID element) {
            if(! activeWorkers.remove(element)){
                throw new IllegalStateException("Expected worker to be active!");
            }
            passiveWorkers.add(element);
        }
    }
}
