package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.files.TaskMeta;
import net.tomp2p.peers.Number160;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.utils.ByteArray;

import java.io.IOException;
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
    private final int EXPECTED_REPUTATION = 3;

    private final int CALENDAR_FIELD;
    private final int CALENDAR_VALUE;

//    private final Deque<Replica> stagedReplicas = new ArrayDeque<>();
    private final Map<String, Replica> replicaMap = new HashMap<>();
    private final Map<String, List<Replica>> finishedReplicasTaskMap = new HashMap<>();

    private final Map<WorkerID, Set<TaskData>> assignedTasks = new HashMap<>();

    private final static Comparator<TaskCompare> TASK_COMPARE = new Comparator<TaskCompare>() {
        @Override
        public int compare(TaskCompare o1, TaskCompare o2) {
            if(o1.value()>o2.value()){
                return 1;
            } else if(o1.value()<o2.value()){
                return -1;
            } else{
                return 0;
            }
        }
    };

    // Used for decision making based on reputation
    private final TreeSet<TaskCompare> taskDatas = new TreeSet<>(TASK_COMPARE);

    private final Map<String, String> jobNameOfTask = new HashMap<>();


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
        //todo update
//        clone.stagedReplicas.addAll(this.stagedReplicas);

        return clone;
    }

    /**
     * Load TaskMeta objects to make replicas of
     *
     * @param tasks List of TaskMeta objects
     */
    public synchronized void loadTasksAndReplicate(String jobName, List<TaskMeta> tasks){
        for(TaskMeta task : tasks){
//            jobNameOfTask.put(task.getTaskName(), jobName);
//            for(int i=0; i<REPLICAS; ++i){
//                Replica replica = new Replica(task);
//                replicaMap.put(replica.getReplicaBox().getReplicaID(), replica);
//                stagedReplicas.addFirst(replica);
//            }
            //TODO fix this
            taskDatas.add(new TaskData(task, jobName, REPLICAS, EXPECTED_REPUTATION));
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
//        Random random = new Random();
//        Replica replica = new Replica(oldReplica.getReplicaBox().getTaskMeta());
//
//        while(replicaMap.containsKey(replica.getReplicaBox().getReplicaID())){
//            replica = new Replica(oldReplica.getReplicaBox().getTaskMeta());
//        }
//        replicaMap.put(replica.getReplicaBox().getReplicaID(), replica);
//        stagedReplicas.addFirst(replica);
        //TODO fix reputation
        int workerReputation = 1;
        //todo implement!
    }

    /**
     *
     * @param worker Worker node
     * @return Replica info if there are any. Returns null if queue is empty.
     *
     */
    public synchronized ReplicaBox giveReplicaToWorker(WorkerID worker){
        final int workerReputation = 1;
        TaskCompare reputationCompare = new TaskCompare() {
            @Override
            public float value() {
                return workerReputation;
            }
        };
        
        Set<TaskData> alreadyGiven = assignedTasks.get(worker);
        if(alreadyGiven == null){
            alreadyGiven = new HashSet<>();
            assignedTasks.put(worker, alreadyGiven);
        }

        //Shallow copy intended
        TreeSet<TaskCompare> notGiven = (TreeSet<TaskCompare>) taskDatas.clone();
        notGiven.removeAll(alreadyGiven);

        if(notGiven.size()==0){
            return null;
        }

        TaskData assign = (TaskData) notGiven.floor(reputationCompare);
        if(assign == null){
            assign = (TaskData) notGiven.ceiling(reputationCompare);
        }

        Replica replica = assign.giveReplica(workerReputation);
        ReplicaBox replicaBox = replica.getReplicaBox();

        alreadyGiven.add(assign);
        replicaTimer.add(replicaBox.getReplicaID(), replicaDeadline());
        replicaMap.put(replicaBox.getReplicaID(), replica);
        return replicaBox;

//        Stack<Replica> skipped = null;
//        try{
//            Replica replica = stagedReplicas.removeLast();
//
//            while(alreadyGiven.contains(replica.getReplicaBox().getTaskMeta())){
//                if(skipped == null){
//                    skipped = new Stack<>();
//                }
//                skipped.push(replica);
//                replica = stagedReplicas.removeLast();
//            }
//            replica.setWorker(worker);
//            alreadyGiven.add(replica.getReplicaBox().getTaskMeta());
//            replicaTimer.add(replica.getReplicaBox().getReplicaID(), replicaDeadline());
//
//            return replica.getReplicaBox();
//        } catch (NoSuchElementException e){
//            //Deque is empty
//            return null;
//        } finally {
//            if(skipped!=null){
//                //Order preserved for skipped replicas
//                while(skipped.size()>0){
//                    Replica r = skipped.pop();
//                    stagedReplicas.addLast(r);
//                }
//            }
//        }
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
        final TaskMeta taskMeta = replica.getReplicaBox().getTaskMeta();
        final String taskName = taskMeta.getTaskName();

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
            validateResults(taskMeta, returnedReplicas);
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
        //TODO implement: note failure as a result. Make comparison later.
    }

    public synchronized Collection<Replica> pendingReplicas(){
        //TODO implement? Want to download results if there have come any to DHT while this job owner was offline
        //TODO Actually this would be better handled by ReplicaTimer... Adding this to Sprint log
        return null;
    }

    public void validateResults(TaskMeta taskMeta, List<Replica> replicaList){
//        String jobName = jobNameOfTask.remove(taskMeta.getTaskName());
        String jobName = "";
        //TODO Use real job name in TaskData!

        Map<ByteArray, List<WorkerID>> resultMap = EqualityControl.compareData(replicaList);
        try {
            Map<ByteArray, Trust> trustMap = QualityControl.compareQuality(jobName, taskMeta, resultMap);
            //TODO Implement actual reward and punishment of peers
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        //TODO Implement choice of automatic or manual result validation
    }
}
