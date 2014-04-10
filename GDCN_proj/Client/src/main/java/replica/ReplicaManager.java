package replica;

import files.TaskMeta;
import net.tomp2p.peers.Number160;
import network.WorkerID;

import java.io.Serializable;
import java.util.*;

/**
 * Created by Leif on 2014-03-31.
 *
 * //TODO reader-writer synchronization instead of common mutex?
 */
public class ReplicaManager implements Serializable, Outdater, Cloneable{

    private final int REPLICAS;
    private final int EXPECTED_RESULTS;

    private final int CALENDAR_FIELD;
    private final int CALENDAR_VALUE;

    private final Deque<Replica> stagedReplicas = new ArrayDeque<>();
    private final Map<String, Replica> replicaMap = new HashMap<>();
    private final Map<String, List<Replica>> finishedReplicasTaskMap = new HashMap<>();

    private final Map<WorkerID, Set<TaskMeta>> assignedTasks = new HashMap<>();

    private ReplicaTimer replicaTimer = null;

    /**
     * ReplicaManager
     *
     * If R replicas are produced, expects R results.
     *
     * @param replicas Number of replicas per task
     */
    public ReplicaManager(int replicas) {
        this(replicas, replicas);
    }

    /**
     *
     * @param replicas Number of replicas per task
     * @param expectedResults Number of expected results, must be less or equal to replicas
     */
    public ReplicaManager(int replicas, int expectedResults) {
        //TODO how long time?
        this(replicas, expectedResults, 5, Calendar.HOUR);
    }

    /**
     * @param replicas Number of replicas per task
     * @param expectedResults Number of expected results, must be less or equal to replicas
     * @param calendarValue Deadline time, see {@link java.util.Calendar#add(int, int)}
     * @param calendarField Deadline time, see {@link java.util.Calendar#add(int, int)}
     */
    public ReplicaManager(int replicas, int expectedResults, int calendarValue, int calendarField){
        this(replicas, expectedResults, calendarValue, calendarField, 1000*60);
    }

    /**
     * Used for testing purposes
     *
     * @param replicas Number of replicas per task
     * @param expectedResults Number of expected results, must be less or equal to replicas
     * @param calendarValue Deadline time, see {@link java.util.Calendar#add(int, int)}
     * @param calendarField Deadline time, see {@link java.util.Calendar#add(int, int)}
     * @param updateInterval Milliseconds for clock to update queue
     */
    public ReplicaManager(int replicas, int expectedResults, int calendarValue, int calendarField, long updateInterval){
        if(replicas<1){
            throw new IllegalArgumentException("Number of replicas must at least 1");
        }
        if(expectedResults > replicas){
            throw new IllegalArgumentException("Number of expected results cannot exceed number of replicas!");
        }
        REPLICAS = replicas;
        EXPECTED_RESULTS = expectedResults;
        CALENDAR_FIELD = calendarField;
        CALENDAR_VALUE = calendarValue;

        replicaTimer = new ReplicaTimer(this, updateInterval);
        resumeTimer();
    }

    /**
     * Must be called after being deserialized for the timer to start running!
     *
     * Is called in constructor.
     */
    public void resumeTimer(){
        replicaTimer.setOutdater(this);

        Thread timerThread = new Thread(replicaTimer.createUpdater());
        timerThread.setDaemon(true);

        timerThread.start();
    }

    /**
     * This is expensive... Use when serializing.
     *
     * Need to use this because ReplicaTimer is only Serializable when its Outdater is null.
     *
     * @return Serializable clone of ReplicaManager
     */
    @Override
    public ReplicaManager clone(){
        ReplicaManager clone = new ReplicaManager(REPLICAS, 2, CALENDAR_VALUE, CALENDAR_FIELD);
        clone.replicaTimer = this.replicaTimer.clone();

        clone.assignedTasks.putAll(this.assignedTasks);
        clone.finishedReplicasTaskMap.putAll(this.finishedReplicasTaskMap);
        clone.replicaMap.putAll(this.replicaMap);
        clone.stagedReplicas.addAll(this.stagedReplicas);

        return clone;
    }

    /**
     * Load TaskMeta objects to make replicas of
     *
     * @param tasks List of TaskMeta objects
     */
    public synchronized void loadTasksAndReplicate(List<TaskMeta> tasks){
        for(TaskMeta task : tasks){
            for(int i=0; i<REPLICAS; ++i){
                Replica replica = new Replica(task, i);
                replicaMap.put(replica.getReplicaBox().getReplicaID(), replica);
                stagedReplicas.addFirst(replica);
            }
        }
    }

    /**
     * This replica didn't get any answer within given time limit. Create another one.
     * Doesn't have to report worker, he might still come up with an answer.
     *
     * If the replica was returned before this is called or if the replicaID doesn't exist, the state is unchanged.
     *
     * @param replicaID Replica that was outdated
     */
    @Override
    public synchronized void replicaOutdated(String replicaID){
        Replica oldReplica = replicaMap.get(replicaID);
        if(oldReplica==null){
            //It might already be returned! Hence not present in replicaMap
            return;
            //throw new IllegalArgumentException("ReplicaID "+replicaID+" doesn't exist so it cannot be outdated!");
        }
        Random random = new Random();
        Replica replica = new Replica(oldReplica.getReplicaBox().getTaskMeta(), random.nextInt());

        while(replicaMap.containsKey(replica.getReplicaBox().getReplicaID())){
            replica = new Replica(oldReplica.getReplicaBox().getTaskMeta(), random.nextInt());
        }
        replicaMap.put(replica.getReplicaBox().getReplicaID(), replica);
        stagedReplicas.addFirst(replica);
    }

    /**
     *
     * @param worker Worker node
     * @return Replica info if there are any. Returns null if queue is empty.
     *
     */
    public synchronized ReplicaBox giveReplicaToWorker(WorkerID worker){
        Set<TaskMeta> alreadyGiven = assignedTasks.get(worker);
        if(alreadyGiven == null){
            alreadyGiven = new HashSet<>();
            assignedTasks.put(worker, alreadyGiven);
        }

        Stack<Replica> skipped = null;
        try{
            Replica replica = stagedReplicas.removeLast();

            while(alreadyGiven.contains(replica.getReplicaBox().getTaskMeta())){
                if(skipped == null){
                    skipped = new Stack<>();
                }
                skipped.push(replica);
                replica = stagedReplicas.removeLast();
            }
            replica.setWorker(worker);
            alreadyGiven.add(replica.getReplicaBox().getTaskMeta());
            replicaTimer.add(replica.getReplicaBox().getReplicaID(), replicaDeadline());

            return replica.getReplicaBox();
        } catch (NoSuchElementException e){
            //Deque is empty
            return null;
        } finally {
            if(skipped!=null){
                //Order preserved for skipped replicas
                while(skipped.size()>0){
                    Replica r = skipped.pop();
                    stagedReplicas.addLast(r);
                }
            }
        }
    }

    private Date replicaDeadline(){
        Calendar calendar = new GregorianCalendar();
        calendar.add(CALENDAR_FIELD, CALENDAR_VALUE);
        return calendar.getTime();
    }

    /**
     *
     * @param replicaID ID of a replica
     * @return Key for the result file in DHT
     */
    public synchronized Number160 getReplicaResultKey(String replicaID){
        final Replica replica = replicaMap.get(replicaID);
        if(replica == null){
            throw new IllegalStateException("Error: Replica was not found!");
        }
        return replica.getReplicaBox().getResultKey();
    }

    /**
     *
     * @param replicaID ID of a replica
     * @param result Computed result of the replica
     */
    public synchronized void replicaFinished(String replicaID, byte[] result){
        if(result == null){
            throw new IllegalArgumentException("Error: don't give null result!");
        }

        final Replica replica = replicaMap.remove(replicaID);
        if(replica == null){
            throw new IllegalStateException("Error: Replica was not found!");
        }

        replica.setResult(result);
        final String taskName = replica.getReplicaBox().getTaskMeta().getTaskName();

        //TODO what if this return is a late-comer? Ie enough replica results have been given already

        List<Replica> returnedReplicas = finishedReplicasTaskMap.get(taskName);
        if(returnedReplicas==null){
            //This is the First replica to return for this task
            List<Replica> list = new ArrayList<>();
            list.add(replica);
            finishedReplicasTaskMap.put(taskName, list);
        } else if(returnedReplicas.size() == EXPECTED_RESULTS-1){
            //This is the Last replica to return for this task
            finishedReplicasTaskMap.remove(taskName);
            returnedReplicas.add(replica);
            validateResults(returnedReplicas);
        } else {
            returnedReplicas.add(replica);
        }
    }

    /**
     *
     * @param workerID Worker
     * @param replicaID ID of a replica
     * @return true only if worker was assigned this replica, otherwise false.
     */
    public synchronized boolean isWorkerAssignedReplica(WorkerID workerID, String replicaID){
        if(workerID==null || replicaID == null){
            return false;
        }
        Replica replica = replicaMap.get(replicaID);
        if(replica == null){
            return false;
        }
        if(! workerID.equals(replica.getWorker())){
            return false;
        }
        return true;
    }

    public synchronized void replicaFailed(String replicaID){
        //TODO implement
    }

    public synchronized Collection<Replica> pendingReplicas(){
        //TODO implement? Want to download results if there have come any to DHT while this job owner was offline
        return null;
    }

    public void validateResults(List<Replica> replicaList){
        //TODO Validate results! Perhaps use interface (ie Strategy pattern)?
    }
}
