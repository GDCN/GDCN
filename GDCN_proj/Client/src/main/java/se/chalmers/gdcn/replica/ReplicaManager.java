package se.chalmers.gdcn.replica;

import net.tomp2p.peers.Number160;
import se.chalmers.gdcn.control.WorkerNodeManager;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.utils.Identifier;
import se.chalmers.gdcn.utils.SerializableTimer;

import java.io.Serializable;
import java.util.*;

/**
 * Created by Leif on 2014-03-31.
 *
 * //TODO reader-writer synchronization instead of common mutex?
 */
public class ReplicaManager implements Serializable{

    private final int REPLICAS;
    private final int EXPECTED_REPUTATION;

    private final int CALENDAR_FIELD;
    private final int CALENDAR_VALUE;

    private final WorkerNodeManager workerNodeManager;
    private final SerializableReplicaTimer replicaTimer;

    private final Map<ReplicaID, Replica> replicaMap = new HashMap<>();
    private final Map<ReplicaID, TaskData> taskDataMap = new HashMap<>();
    private final Map<TaskID, TaskResultData> resultDataMap = new HashMap<>();

    private final Map<WorkerID, Set<TaskData>> assignedTasks = new HashMap<>();
    private final TreeSet<TaskCompare> taskDatas = new TreeSet<>(new TaskComparator()); // Used for decision making based on reputation


    private static class TaskComparator implements Comparator<TaskCompare>, Serializable {
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
    }

    public static class ReplicaID extends Identifier{
        public ReplicaID(String id) {
            super(id);
        }
    }

    public static class TaskID extends Identifier{
        public TaskID(String id) {
            super(id);
        }
    }

    /**
     * Contains information about the status of a task, each String below is a ReplicaID
     */
    private static class TaskResultData implements Serializable{
        final Set<ReplicaID> failedReplicas = new HashSet<>();
        final Set<ReplicaID> outdatedReplicas = new HashSet<>();
        final Set<ReplicaID> pendingReplicas = new HashSet<>();
        final Map<ReplicaID, byte[]> returnedReplicas = new HashMap<>();
    }


    public ReplicaManager(WorkerID myWorkerID, int calendarValue, int calendarField, long updateInterval){
        this(new WorkerNodeManager(myWorkerID), calendarValue, calendarField, updateInterval);
    }

    public ReplicaManager(WorkerNodeManager workerNodeManager, int calendarValue, int calendarField, long updateInterval){

        //TODO stop hardcoding values, make Builder class later
        REPLICAS = 2;
        EXPECTED_REPUTATION = 3;
        CALENDAR_FIELD = calendarField;
        CALENDAR_VALUE = calendarValue;

        replicaTimer = new SerializableReplicaTimer(updateInterval);
        this.workerNodeManager = workerNodeManager;

        resumeTimer();
    }

    public ReplicaManager(WorkerNodeManager workerNodeManager, int calendarValue, int calendarField, long updateInterval, int replicas, int expectedReputation){

        //TODO stop hardcoding values, make Builder class later
        REPLICAS = replicas;
        EXPECTED_REPUTATION = expectedReputation;
        CALENDAR_FIELD = calendarField;
        CALENDAR_VALUE = calendarValue;

        replicaTimer = new SerializableReplicaTimer(updateInterval);
        this.workerNodeManager = workerNodeManager;

        resumeTimer();
    }

    /**
     * Must be called after being deserialized for the timer to start running!
     * Is called in constructor.
     */
    public void resumeTimer(){
        Thread timerThread = new Thread(replicaTimer.createUpdater());
        timerThread.setDaemon(true);

        timerThread.start();
    }

    /**
     * Load TaskMeta objects to make replicas of
     * @param tasks List of TaskMeta objects
     */
    public synchronized void loadTasksAndReplicate(String jobName, List<TaskMeta> tasks){
        for(TaskMeta task : tasks){
            taskDatas.add(new TaskData(task, jobName, REPLICAS, EXPECTED_REPUTATION));
        }
    }

    /**
     * @param worker Worker node
     * @return Replica info if there are any. Returns null if queue is empty.
     *
     */
    public synchronized ReplicaBox giveReplicaToWorker(WorkerID worker){
        Set<TaskData> alreadyGiven = assignedTasks.get(worker);
        if(alreadyGiven == null){
            alreadyGiven = new HashSet<>();
            assignedTasks.put(worker, alreadyGiven);
        }

        //Shallow copy intended
        TreeSet<TaskCompare> notGiven = (TreeSet<TaskCompare>) taskDatas.clone();
        notGiven.removeAll(alreadyGiven);
        if(notGiven.size()==0){
            //No task left to work on for that worker
            return null;
        }

        final int workerReputation = workerNodeManager.getReputation(worker);
        TaskCompare reputationCompare = new TaskCompare() {
            @Override
            public float value() {
                return workerReputation;
            }
        };
        //Assign task in a smart manner
        TaskData taskData = (TaskData) notGiven.floor(reputationCompare);
        if(taskData == null){
            //Warning, might not fulfill reputation demand!
            //TODO check condition!
            taskData = (TaskData) notGiven.ceiling(reputationCompare);
        }

        TaskMeta taskMeta = taskData.giveTask(workerReputation);
        ReplicaBox replicaBox = new ReplicaBox(taskMeta);
        while (replicaMap.containsKey(replicaBox.getReplicaID())){
            replicaBox = new ReplicaBox(taskMeta);
        }

        final ReplicaID replicaID = replicaBox.getReplicaID();

        //Update state:
        alreadyGiven.add(taskData);
        taskDataMap.put(replicaID, taskData);
        replicaTimer.add(replicaID, replicaDeadline());
        replicaMap.put(replicaID, new Replica(replicaBox, worker));

        TaskResultData taskResultData = resultDataMap.get(taskData.taskID());
        if(taskResultData == null){
            taskResultData = new TaskResultData();
            resultDataMap.put(taskData.taskID(), taskResultData);
        }
        taskResultData.pendingReplicas.add(replicaID);

        return replicaBox;
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
    public synchronized Number160 getReplicaResultKey(ReplicaID replicaID){
        final Replica replica = replicaMap.get(replicaID);
        if(replica == null){
            throw new IllegalStateException("Error: Replica was not found!");
        }
        return replica.getReplicaBox().getResultKey();
    }


    private TaskResultData returned(ReplicaID replicaID){
        //TODO handle latecomer
        TaskData taskData = taskDataMap.get(replicaID);
        if(taskData == null){
            throw new IllegalStateException("Couldn't find TaskData in taskDataMap!");
        }

        TaskResultData resultData = resultDataMap.get(taskData.taskID());
//        System.out.println("Returned: "+replicaID);
//        for(ReplicaID r : resultData.pendingReplicas){
//            System.out.println("\t"+r);
//        }
        if(! resultData.pendingReplicas.remove(replicaID)){
            if(! resultData.outdatedReplicas.remove(replicaID)){
                throw new IllegalStateException("Expected replicaID to be in pendingReplicas or outdatedReplicas!");
            }
        }
        return resultData;
    }

    /**
     * Should only be used for testing! Called internally.
     *
     * This replica didn't get any answer within given time limit.
     * Doesn't have to report worker, he might still come up with an answer.
     * @param replicaID Replica that was outdated
     */
    public synchronized void replicaOutdated(ReplicaID replicaID){
        TaskResultData resultData = returned(replicaID);
        resultData.outdatedReplicas.add(replicaID);
        decideValidate(replicaID);
    }

    public synchronized void replicaFailed(ReplicaID replicaID){
        TaskResultData resultData = returned(replicaID);
        resultData.failedReplicas.add(replicaID);
        decideValidate(replicaID);
    }

    public synchronized void replicaFinished(ReplicaID replicaID, byte[] result){
        if(result == null){
            throw new IllegalArgumentException("Error: don't give null result!");
        }
        TaskResultData resultData = returned(replicaID);
        resultData.returnedReplicas.put(replicaID, result);
        decideValidate(replicaID);
    }

    private void decideValidate(ReplicaID replicaID){
        //Make sure timeout will not be called on this replicaID
        replicaTimer.remove(replicaID);
        //TODO validate now or wait?
    }

    /**
     *
     * @param workerID Worker
     * @param replicaID ID of a replica
     * @return true only if worker was assigned this replica, otherwise false.
     */
    public synchronized boolean isWorkerAssignedReplica(WorkerID workerID, ReplicaID replicaID){
        if(workerID==null || replicaID == null){
            return false;
        }
        Replica replica = replicaMap.get(replicaID);
        return replica != null && replica.getWorker().equals(workerID);
    }

    public synchronized Set<ReplicaID> pendingReplicaIDs(){
        Set<ReplicaID> replicaIDs = new HashSet<>();
        for(TaskResultData taskResultData : resultDataMap.values()){
            replicaIDs.addAll(taskResultData.pendingReplicas);
        }
        return replicaIDs;
    }

    /**
     * This method is package access only since Replica is package access only...
     * //TODO make public? What is really wanted is the resultKey for each replica.
     * @return Set of pending replicas
     */
    synchronized Set<Replica> pendingReplicas(){
        Set<Replica> replicas = new HashSet<>();
        for(ReplicaID replicaID : pendingReplicaIDs()){
            replicas.add(replicaMap.get(replicaID));
        }
        return replicas;
    }

    public void validateResults(TaskMeta taskMeta, List<Replica> replicaList){
//        String jobName = jobNameOfTask.remove(taskMeta.getTaskName());
        String jobName = "";
        //TODO Use real job name in TaskData!
//
//        Map<ByteArray, List<WorkerID>> resultMap = EqualityControl.compareData(replicaList);
//        try {
//            Map<ByteArray, Trust> trustMap = QualityControl.compareQuality(jobName, taskMeta, resultMap);
//            //TODO Implement actual reward and punishment of peers
//        }
//        catch (IOException e) {
//            e.printStackTrace();
//        }
        //TODO Implement choice of automatic or manual result validation
    }

    private class SerializableReplicaTimer extends SerializableTimer<ReplicaID>{
        /**
         * @param updateTime Number of Milliseconds between check queue
         */
        public SerializableReplicaTimer(long updateTime) {
            super(updateTime);
        }

        @Override
        protected void handleTimeout(ReplicaID element) {
            //TODO timeout will always be called now
            ReplicaManager.this.replicaOutdated(element);
        }
    }

}
