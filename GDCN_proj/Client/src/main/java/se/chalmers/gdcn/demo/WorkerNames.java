package se.chalmers.gdcn.demo;

import se.chalmers.gdcn.network.WorkerID;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by Leif on 2014-05-24.
 */
public class WorkerNames{
    private final Map<WorkerID, String> names = new HashMap<>();
    private Integer counter = 0;
    private WorkerID myself = null;

    private final static WorkerNames instance = new WorkerNames();

    public static WorkerNames getInstance() {
        return instance;
    }

    /**
     * Give worker a local nick name
     * @param workerID worker
     * @return true if worker was added, false if worker was already known
     */
    public boolean registerName(WorkerID workerID){
        if(names.keySet().contains(workerID)){
            return false;
        }

        synchronized (names){
            names.put(workerID, "Worker_"+counter);
            counter++;
        }
        return true;
    }

    /**
     * Get local nick name for worker.
     * @param workerID worker
     * @return local nick name
     */
    public String getName(WorkerID workerID){
        synchronized (names){
            return names.get(workerID);
        }
    }

    /**
     * Set workerID of this local peer.
     * @param workerID myself
     */
    public void setLocalID(WorkerID workerID){
        if(myself.equals(workerID)){
            return;
        }

        final WorkerID oldMe = myself;
        synchronized (names){
            names.remove(workerID);
            names.remove(myself);
            names.put(workerID, "Myself");
        }
        registerName(oldMe);
    }


}
