package replica;

import control.WorkerNodeManager;

import java.util.*;

/**
 * Created by Leif on 2014-03-31.
 *
 * //TODO save this object to file on 'exit' and load it from file on startup
 *
 * //TODO reissue replicas after timeout time
 */
public class ReplicaManager {

    private final int REPLICAS;

    private final Deque<Replica> stagedReplicas = new ArrayDeque<>();
    private final Map<String, Replica> replicaMap = new HashMap<>();
    private final Map<String, List<Replica>> finishedReplicasTaskMap = new HashMap<>();

    private final Map<WorkerNodeManager.WorkerID, Set<String>> assignedTasks = new HashMap<>();

    public ReplicaManager(int replicas) {
        REPLICAS = replicas;
    }

    /**
     * Load in TaskMeta objects
     *
     * TODO send real Task information!
     */
    public synchronized void loadTasksAndReplicate(List<String> tasks){
        for(String task : tasks){
            for(int i=0; i<REPLICAS; ++i){
                Replica replica = new Replica(task, i);
                replicaMap.put(replica.getReplicaID(), replica);
                stagedReplicas.addFirst(replica);
            }
        }
    }

    /**
     *
     * @param worker Worker node
     * @return String with replica info if exist. Returns null if queue is empty.
     *
     * //TODO send real Task information!
     */
    public synchronized String giveReplicaToWorker(WorkerNodeManager.WorkerID worker){
        Set<String> alreadyGiven = assignedTasks.get(worker);
        if(alreadyGiven == null){
            alreadyGiven = new HashSet<>();
            assignedTasks.put(worker, alreadyGiven);
        }

        try{
            Replica replica = stagedReplicas.removeLast();
            Stack<Replica> skipped = null;
            while(alreadyGiven.contains(replica.getTaskID())){
                if(skipped == null){
                    skipped = new Stack<>();
                }
                skipped.push(replica);
                replica = stagedReplicas.removeLast();
            }
            if(skipped!=null){
                //Order preserved for skipped replicas
                while(skipped.size()>0){
                    Replica r = skipped.pop();
                    stagedReplicas.addLast(r);
                }
            }

            replica.setWorker(worker);
            alreadyGiven.add(replica.getTaskID());

            return replica.getReplicaID();
        } catch (NoSuchElementException e){
            //Deque is empty
            return null;
        }
    }

    public synchronized void replicaFinished(String replicaID, Object result){
        final Replica replica = replicaMap.remove(replicaID);
        if(replica == null){
            throw new IllegalStateException("Error: Replica was not found!");
        }
        replica.setResult(result);

        List<Replica> returnedReplicas = finishedReplicasTaskMap.get(replica.getTaskID());
        if(returnedReplicas==null){
            //This is the First replica to return for this task
            List<Replica> list = new ArrayList<>();
            list.add(replica);
        } else if(returnedReplicas.size()==REPLICAS-1){
            //This is the Last replica to return for this task
            finishedReplicasTaskMap.remove(replica.getTaskID());
            returnedReplicas.add(replica);
            validateResults(returnedReplicas);
        } else {
            returnedReplicas.add(replica);
        }
    }

    public void validateResults(List<Replica> replicaList){
        //TODO Validate results! Perhaps use interface (ie Strategy pattern)?
    }
}
