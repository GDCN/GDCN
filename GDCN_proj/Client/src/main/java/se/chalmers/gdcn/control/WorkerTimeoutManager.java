package se.chalmers.gdcn.control;

import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.utils.SerializableTimer;

import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by Leif on 2014-04-21.
 */
public class WorkerTimeoutManager implements Serializable{

    private final Set<WorkerID> activeWorkers = new HashSet<>();
    private final Set<WorkerID> passiveWorkers = new HashSet<>();

    private final WorkTimer workTimer;

    public WorkerTimeoutManager(long updateTime) {
        workTimer = new WorkTimer(updateTime);
    }

    public void activate(WorkerID workerID){
        if(passiveWorkers.remove(workerID)){
            activeWorkers.add(workerID);
        }
        workTimer.reset(workerID, new Date());
    }

    public void resumeTimer(){
        SerializableTimer.resume(workTimer);
    }

    private class WorkTimer extends SerializableTimer<WorkerID>{

        /**
         * @param updateTime Number of Milliseconds between check queue
         */
        public WorkTimer(long updateTime) {
            super(updateTime);
        }

        @Override
        protected void handleTimeout(WorkerID element) {
            //todo
        }
    }
}
