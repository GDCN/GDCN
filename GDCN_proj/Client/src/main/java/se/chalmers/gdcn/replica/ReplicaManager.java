package se.chalmers.gdcn.replica;

import net.tomp2p.peers.Number160;
import se.chalmers.gdcn.compare.EqualityControl;
import se.chalmers.gdcn.compare.QualityControl;
import se.chalmers.gdcn.compare.Trust;
import se.chalmers.gdcn.control.TaskRunner;
import se.chalmers.gdcn.control.WorkerReputationManager;
import se.chalmers.gdcn.control.WorkerTimeoutManager;
import se.chalmers.gdcn.files.FileManagementUtils;
import se.chalmers.gdcn.files.SelfWorker;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.files.TaskMetaDataException;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.taskbuilder.Task;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
import se.chalmers.gdcn.utils.ByteArray;
import se.chalmers.gdcn.utils.Identifier;
import se.chalmers.gdcn.utils.SerializableTimer;
import se.chalmers.gdcn.utils.Time;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.*;

/**
 * Created by Leif on 2014-03-31.
 *
 * //TODO reader-writer synchronization instead of common mutex?
 */
public class ReplicaManager implements Serializable, Cloneable{

    private final int REPLICAS;
    private final int EXPECTED_REPUTATION;

    private final Time TIME_UNIT;
    private final int CALENDAR_VALUE;

    private transient TaskRunner runner;

    private final WorkerReputationManager workerReputationManager;
    private final WorkerTimeoutManager workerTimeoutManager;
    private final SerializableReplicaTimer replicaTimer;

    private final Map<ReplicaID, Replica> replicaMap = new HashMap<>();
    private final Map<ReplicaID, TaskData> taskDataMap = new HashMap<>();
    private final Map<TaskID, TaskResultData> resultDataMap = new HashMap<>();

    private final Map<WorkerID, Set<TaskData>> assignedTasks = new HashMap<>();
    private final TreeSet<TaskCompare> taskDatas = new TreeSet<>(new TaskComparator()); // Used for decision making based on reputation

    private boolean workSelfIfRequired = true;

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
     * Contains information about the status of a task
     */
    private static class TaskResultData implements Serializable{
        final Set<ReplicaID> failedReplicas = new HashSet<>();
        final Set<ReplicaID> outdatedReplicas = new HashSet<>();
        final Set<ReplicaID> pendingReplicas = new HashSet<>();
        final Map<ReplicaID, byte[]> returnedReplicas = new HashMap<>();
    }


    /**
     * Please use {@link se.chalmers.gdcn.replica.ReplicaManagerBuilder} for constructing this class
     */
    ReplicaManager(WorkerReputationManager workerReputationManager, TaskRunner runner, Time timeUnit, long updateInterval, int replicas, int expectedReputation, int calendarValue){
        REPLICAS = replicas;
        EXPECTED_REPUTATION = expectedReputation;
        TIME_UNIT = timeUnit;
        CALENDAR_VALUE = calendarValue;

        replicaTimer = new SerializableReplicaTimer(updateInterval);
        this.workerReputationManager = workerReputationManager;
        //TODO calibrate: are these acceptable values?
        workerTimeoutManager = new WorkerTimeoutManager(updateInterval*2, timeUnit, calendarValue*3);

        this.runner = runner;
        resumeTimer();
    }

    public synchronized void setTaskManager(TaskRunner taskManager) {
        this.runner = taskManager;
    }

    public TaskRunner getRunner() {
        return runner;
    }

    public WorkerReputationManager getWorkerReputationManager() {
        return workerReputationManager;
    }

    /**
     * Mainly intended for testing
     * @param workSelfIfRequired true if allow JobOwner to work himself if there are too few active workers
     */
    public synchronized void setWorkSelfIfRequired(boolean workSelfIfRequired) {
        this.workSelfIfRequired = workSelfIfRequired;
    }

    /**
     * Must be called after being deserialized for the timer to start running!
     * Is called in constructor.
     */
    public void resumeTimer(){
        if(runner != null){
            runner.submit(replicaTimer.createUpdater());
            runner.submit(workerTimeoutManager.timerRunner());
        } else {
            //In testing...
            SerializableTimer.resume(replicaTimer);
//            workerTimeoutManager.resumeTimer();
        }
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
        workerTimeoutManager.activate(worker);

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

        final int workerReputation = workerReputationManager.getReputation(worker);
        TaskCompare reputationCompare = new TaskCompare() {
            @Override
            public float value() {
                return workerReputation;
            }
            @Override
            public String order() {
                return "";
            }
        };
        //Assign task in a smart manner
        TaskData taskData = (TaskData) notGiven.floor(reputationCompare);
        if(taskData == null){
            //Warning, might not fulfill reputation demand!
            //TODO check condition!
            taskData = (TaskData) notGiven.ceiling(reputationCompare);
        }

        taskDatas.remove(taskData);
        //TaskData changes state internally which affects its sorted position! Remove and insert!
        TaskMeta taskMeta = taskData.giveTask(worker, workerReputation);
        taskDatas.add(taskData);

        ReplicaBox replicaBox = new ReplicaBox(taskMeta);
        while (replicaMap.containsKey(replicaBox.getReplicaID())){
            replicaBox = new ReplicaBox(taskMeta);
        }

        final ReplicaID replicaID = replicaBox.getReplicaID();

        //Update state:
        alreadyGiven.add(taskData);
        taskDataMap.put(replicaID, taskData);
        replicaMap.put(replicaID, new Replica(replicaBox, worker));

        Date deadline = Time.futureDate(this.TIME_UNIT, CALENDAR_VALUE);
        replicaTimer.add(replicaID, deadline);

        TaskResultData taskResultData = resultDataMap.get(taskData.taskID());
        if(taskResultData == null){
            taskResultData = new TaskResultData();
            resultDataMap.put(taskData.taskID(), taskResultData);
        }
        taskResultData.pendingReplicas.add(replicaID);

        return replicaBox;
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

//    private TaskResultData returned(ReplicaID replicaID){
//        //TODO handle latecomer
//        TaskData taskData = taskDataMap.get(replicaID);
//        if(taskData == null){
//            throw new IllegalStateException("Couldn't find TaskData in taskDataMap!");
//        }
//
//        TaskResultData resultData = resultDataMap.get(taskData.taskID());
//
//        if(! resultData.pendingReplicas.remove(replicaID)){
//            if(! resultData.outdatedReplicas.remove(replicaID)){
//                throw new IllegalStateException("Expected replicaID to be in pendingReplicas or outdatedReplicas!");
//            }
//        }
//        return resultData;
//    }

    /**
     * This method should only be used externally for testing!
     * Is called internally.
     *
     * This replica didn't get any answer within given time limit.
     * Doesn't have to report worker, he might still come up with an answer.
     * @param replicaID Replica that was outdated
     */
    public synchronized void replicaOutdated(ReplicaID replicaID){
        TaskData taskData = taskDataMap.get(replicaID);
        if(taskData == null){
            throw new IllegalStateException("Couldn't find TaskData in taskDataMap!");
        }

        TaskResultData resultData = resultDataMap.get(taskData.taskID());

        if(! resultData.pendingReplicas.remove(replicaID)){
            if(! resultData.outdatedReplicas.remove(replicaID)){
                throw new IllegalStateException("Expected replicaID to be in pendingReplicas or outdatedReplicas!");
            }
        }

        taskDatas.remove(taskData);
        taskData.timedOut(replicaMap.get(replicaID).getWorker());
        taskDatas.add(taskData);

        resultData.outdatedReplicas.add(replicaID);
        decideValidate(replicaID, taskData, resultData);
    }

    public synchronized void replicaFailed(ReplicaID replicaID){
        TaskData taskData = taskDataMap.get(replicaID);
        if(taskData == null){
            throw new IllegalStateException("Couldn't find TaskData in taskDataMap!");
        }

        TaskResultData resultData = resultDataMap.get(taskData.taskID());

        if(! resultData.pendingReplicas.remove(replicaID)){
            if(! resultData.outdatedReplicas.remove(replicaID)){
                throw new IllegalStateException("Expected replicaID to be in pendingReplicas or outdatedReplicas!");
            }
        }

        taskDatas.remove(taskData);
        taskData.returned(replicaMap.get(replicaID).getWorker());
        taskDatas.add(taskData);

        resultData.failedReplicas.add(replicaID);
        decideValidate(replicaID, taskData, resultData);
    }

    public synchronized void replicaFinished(ReplicaID replicaID, byte[] result){
        if(result == null){
            throw new IllegalArgumentException("Error: don't give null result!");
        }
        TaskData taskData = taskDataMap.get(replicaID);
        if(taskData == null){
            throw new IllegalStateException("Couldn't find TaskData in taskDataMap!");
        }

        TaskResultData resultData = resultDataMap.get(taskData.taskID());

        if(! resultData.pendingReplicas.remove(replicaID)){
            if(! resultData.outdatedReplicas.remove(replicaID)){
                throw new IllegalStateException("Expected replicaID to be in pendingReplicas or outdatedReplicas!");
            }
        }
        taskDatas.remove(taskData);
        taskData.returned(replicaMap.get(replicaID).getWorker());
        taskDatas.add(taskData);

        resultData.returnedReplicas.put(replicaID, result);
        decideValidate(replicaID, taskData, resultData);
    }

    private void decideValidate(ReplicaID replicaID, TaskData taskData, TaskResultData resultData){
        //Make sure timeout will not be called on this replicaID:
        replicaTimer.remove(replicaID);

        if( workSelfIfRequired && !isThereTaskWithEnoughReputationAlready() && sumActiveReputation() < EXPECTED_REPUTATION){
            workSelf(taskData);
        }

        if(! taskData.enoughReturned()){
            //Ignore - cannot validate yet
            return;
        }
        //Can validate

        if(resultData.pendingReplicas.size() > 0){
            //TODO wait for them to return/timeout?
        }

        validateResults(taskData, resultData);
    }

    private float sumActiveReputation(){
        float sum = 0;
        for( WorkerID workerID : workerTimeoutManager.getActiveWorkers() ){
            sum += workerReputationManager.getReputation(workerID);
        }
        return sum;
    }

    private void workSelf(final TaskData taskData){
        TaskMeta meta = taskData.getTaskMeta();

        //todo nicer replicaID
        //todo reuse giveReplica method?
        final ReplicaID replicaID = new ReplicaID("SelfWork_"+meta.getTaskName());

        taskDatas.remove(taskData);
        taskData.giveTask(workerReputationManager.getMyWorkerID(), Float.MAX_VALUE);
        taskDatas.add(taskData);

        try {
            SelfWorker selfWorker = new SelfWorker(meta, taskData.getJobName());
            final String resultPath = selfWorker.futureResultFilePath();
            final TaskResultData taskResultData = resultDataMap.get(taskData.taskID());

            Task taskRunner = selfWorker.workSelf(meta, new TaskListener() {
                @Override
                public void taskFinished(String taskName) {
                    System.out.println("ReplicaManager#workSelf - YAY, "+taskName+ "finished");

                    try {
                        byte[] result = FileManagementUtils.fromFile(new File(resultPath));

                        //TODO put jobOwner result in special position?
                        taskResultData.returnedReplicas.put(replicaID, result);
                        runner.getTaskListener().taskFinished(taskName);

                        decideValidate(replicaID, taskData, taskResultData);

                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }

                @Override
                public void taskFailed(String taskName, String reason) {
                    System.out.println("ERROR "+taskName+": "+reason);
                    //TODO report error to UI, use TaskManager for that?
                    taskResultData.failedReplicas.add(replicaID);
                    runner.getTaskListener().taskFailed(taskName, reason);

                    decideValidate(replicaID, taskData, taskResultData);
                }
            });

            runner.submit(taskRunner);
        } catch (TaskMetaDataException e) {
            e.printStackTrace();
        }
    }

    /**
     * @return true if there is a task that has enough reputation worked on it
     */
    private boolean isThereTaskWithEnoughReputationAlready(){
        return taskDatas.first().value() < 0;
    }

    private void validateResults(TaskData taskData, TaskResultData resultData){
        String jobName = taskData.getJobName();

        Map<ByteArray, Set<ReplicaID>> resultMap = EqualityControl.compareData(resultData.returnedReplicas);
        try {
            Map<ByteArray, Trust> trustMap = QualityControl.compareQuality(jobName, taskData.getTaskMeta(), resultMap);
            //TODO Implement actual reward and punishment of peers
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        //TODO Implement choice of automatic or manual result validation
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
     * @return Map with pending replicas and respective location key. Results may or not be uploaded in DHT
     */
    public synchronized Map<ReplicaID, Number160> pendingResults(){
        Map<ReplicaID, Number160> pending = new HashMap<>();
        for(ReplicaID replicaID : pendingReplicaIDs()){
            Replica replica = replicaMap.get(replicaID);
            pending.put(replicaID, replica.getReplicaBox().getResultKey());
        }
        return pending;
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
            ReplicaManager.this.replicaOutdated(element);
        }
    }

}
