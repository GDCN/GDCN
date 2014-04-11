package control;

import network.WorkerID;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by Leif on 2014-03-31.
 *
 * Class to keep track of what peers are registered as workers.
 *
 */
public class WorkerNodeManager implements Serializable{

    public static enum DisciplinaryAction{
        REMOVE,
        DEMOTE
    }

    private final DisciplinaryAction standardAction;
    private final int removeSoManyPoints;

    private final Map<WorkerID, Integer> registeredWorkers = new HashMap<>();

    /**
     * Create new WorkerNodeManager with default action REMOVE.
     */
    public WorkerNodeManager(){
        this(DisciplinaryAction.REMOVE, 3);
    }

    /**
     *
     * @param standardAction Default action to do when reporting
     * @param removeSoManyPoints So many points are withdrawn from a workers reputation when it misbehaves.
     */
    public WorkerNodeManager(DisciplinaryAction standardAction, int removeSoManyPoints) {
        this.standardAction = standardAction;
        this.removeSoManyPoints = removeSoManyPoints;
    }

    /**
     * Registers worker as a worker node to remember
     * @param worker Worker node
     * @return true if worker was added, false if existed already
     */
    public boolean registerWorker(WorkerID worker){
        if(registeredWorkers.keySet().contains(worker)){
            return false;
        }
        registeredWorkers.put(worker, 0);
        return true;
    }

    /**
     *
     * @param worker Worker node
     * @return true if and only if worker is registered
     */
    public boolean isWorkerRegistered(WorkerID worker){
        return registeredWorkers.keySet().contains(worker);
    }

    /**
     * Worker misbehaved. Perhaps it is a Sybil node? Apply standard action.
     * @param worker Worker node
     */
    public void reportWorker(WorkerID worker){
        reportWorker(worker, standardAction);
    }

    /**
     * Worker misbehaved. Perhaps it is a Sybil node?
     * @param worker Worker node
     * @param action Action to take on misbehavior
     */
    public void reportWorker(WorkerID worker, DisciplinaryAction action){
        if(!registeredWorkers.containsKey(worker)){
            //This can happen when a Worker is reported for not computing the registration task properly.
            return;
//            throw new IllegalArgumentException("Worker doesn't exist");
        }

        switch (action){
            case REMOVE:
                registeredWorkers.remove(worker);
                break;
            case DEMOTE:
                if(removeSoManyPoints <= 0){
                    throw new IllegalArgumentException("If use REMOVE, one must subtract points, not give points, on failure.");
                }
                Integer reputation = registeredWorkers.get(worker);
                registeredWorkers.put(worker, reputation-removeSoManyPoints);
                break;
        }
    }

    /**
     * Worker finished some task nicely. Promote the worker.
     * @param worker Worker node
     */
    public void promoteWorker(WorkerID worker){
        if(!registeredWorkers.containsKey(worker)){
            throw new IllegalArgumentException("Worker doesn't exist");
        }

        Integer reputation = registeredWorkers.get(worker);
        registeredWorkers.put(worker, reputation+1);
    }

    /**
     *
     * @param worker Worker node
     * @return Current reputation
     */
    public int getReputation(WorkerID worker){
        if(!registeredWorkers.containsKey(worker)){
            throw new IllegalArgumentException("Worker doesn't exist");
        }

        return registeredWorkers.get(worker);
    }

}
