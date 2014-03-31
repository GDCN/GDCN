package control;

import net.tomp2p.peers.PeerAddress;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by Leif on 2014-03-31.
 *
 * Class to keep track of what peers are registered as workers.
 *
 * //TODO save this object to file on 'exit' and load it from file on startup
 */
public class WorkerNodeManager {

    // TODO replace this WorkerID class with more proper one such as PeerAddress or whatever is chosen
    public static class WorkerID {
        private final PeerAddress address;

        public WorkerID(PeerAddress address){
            this.address = address;
        }
    }

    public static enum DisciplinaryAction{
        REMOVE,
        DEMOTE
    }

    private final DisciplinaryAction standardAction;

    private final Map<WorkerID, Integer> registeredWorkers = new HashMap<>();

    public WorkerNodeManager(DisciplinaryAction standardAction) {
        this.standardAction = standardAction;
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
        switch (action){
            case REMOVE:
                registeredWorkers.remove(worker);
                break;
            case DEMOTE:
                Integer reputation = registeredWorkers.get(worker);
                // TODO remove more than 1 point?
                registeredWorkers.put(worker, reputation-1);
                break;
        }
    }

    /**
     * Worker finished some task nicely. Promote the worker.
     * @param worker Worker node
     */
    public void promoteWorker(WorkerID worker){
        Integer reputation = registeredWorkers.get(worker);
        registeredWorkers.put(worker, reputation+1);
    }

}
