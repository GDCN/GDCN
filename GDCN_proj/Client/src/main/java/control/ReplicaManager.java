package control;

import java.util.*;

/**
 * Created by Leif on 2014-03-31.
 *
 * //TODO save this object to file on 'exit' and load it from file on startup
 */
public class ReplicaManager {

    public class TaskReplica{
        private final String replicaID;

        public TaskReplica(String taskID, int index) {
            this.replicaID = taskID + index;
            //TODO hash or something, should not be so obvious, or should it?
        }
    }

    private final int REPLICAS;
    private final Deque<TaskReplica> stagedReplicas = new ArrayDeque<>();
    private final Map<WorkerNodeManager.WorkerID, TaskReplica> pendingReplicas = new HashMap<>();

    public ReplicaManager(int replicas) {
        REPLICAS = replicas;
    }

    /**
     * Load in TaskMeta objects
     *
     * TODO send real Task information!
     */
    public void loadTasksAndReplicate(List<String> tasks){
        for(String task : tasks){
            for(int i=0; i<REPLICAS; ++i){
                stagedReplicas.addFirst(new TaskReplica(task, i));
            }
        }
    }

    public String giveReplicaToWorker(WorkerNodeManager.WorkerID worker){
        TaskReplica task = stagedReplicas.removeLast();
        pendingReplicas.put(worker, task);
        return task.replicaID;
    }
}
