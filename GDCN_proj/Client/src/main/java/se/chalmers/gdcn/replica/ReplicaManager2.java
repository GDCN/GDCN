package se.chalmers.gdcn.replica;

import net.tomp2p.peers.Number160;
import se.chalmers.gdcn.control.WorkerNodeManager;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.utils.ByteArray;
import se.chalmers.gdcn.utils.Identifier;

import java.io.IOException;
import java.io.Serializable;
import java.util.*;

/**
 * Created by Leif on 2014-03-31.
 *
 * //TODO reader-writer synchronization instead of common mutex?
 */
public class ReplicaManager2 implements Serializable{

    private final int REPLICAS;
    private final int EXPECTED_REPUTATION;

    private final int CALENDAR_FIELD;
    private final int CALENDAR_VALUE;

    private final WorkerNodeManager workerNodeManager;
    private ReplicaTimer2 replicaTimer = null;

    private final Map<ReplicaID, Replica2> replicaMap = new HashMap<>();
    private final Map<String, TaskData> taskDataMap = new HashMap<>();
    private final Map<String, TaskResultData> resultDataMap = new HashMap<>();
//    private final Map<String, List<Replica>> finishedReplicasTaskMap = new HashMap<>();

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

    /**
     * Contains information about the status of a task, each String below is a ReplicaID
     */
    private static class TaskResultData{
        final Set<ReplicaID> failedReplicas = new HashSet<>();
        final Set<ReplicaID> outdatedReplicas = new HashSet<>();
        final Set<ReplicaID> pendingReplicas = new HashSet<>();
        final Map<ReplicaID, byte[]> returnedReplicas = new HashMap<>();
    }


    public ReplicaManager2(WorkerID myWorkerID, int calendarValue, int calendarField, long updateInterval){
        this(new WorkerNodeManager(myWorkerID), calendarValue, calendarField, updateInterval);
    }

    public ReplicaManager2(WorkerNodeManager workerNodeManager, int calendarValue, int calendarField, long updateInterval){

        //TODO stop hardcoding values, make Builder class later
        REPLICAS = 2;
        EXPECTED_REPUTATION = 3;
        CALENDAR_FIELD = calendarField;
        CALENDAR_VALUE = calendarValue;

        replicaTimer = new ReplicaTimer2(updateInterval);
        this.workerNodeManager = workerNodeManager;

        resumeTimer();
    }

    /**
     * Must be called after being deserialized for the timer to start running!
     *
     * Is called in constructor.
     */
    public void resumeTimer(){
        Thread timerThread = new Thread(replicaTimer.createUpdater());
        timerThread.setDaemon(true);

        timerThread.start();
    }

    /**
     * Load TaskMeta objects to make replicas of
     *
     * @param tasks List of TaskMeta objects
     */
    public synchronized void loadTasksAndReplicate(String jobName, List<TaskMeta> tasks){
        for(TaskMeta task : tasks){
            taskDatas.add(new TaskData(task, jobName, REPLICAS, EXPECTED_REPUTATION));
        }
    }

    /**
     *
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
        TaskData assign = (TaskData) notGiven.floor(reputationCompare);
        if(assign == null){
            //Warning, might not fulfill reputation demand!
            assign = (TaskData) notGiven.ceiling(reputationCompare);
        }

        TaskMeta taskMeta = assign.giveTask(workerReputation);
        ReplicaBox replicaBox = new ReplicaBox(taskMeta);
        while (replicaMap.containsKey(replicaBox.getReplicaID())){
            replicaBox = new ReplicaBox(taskMeta);
        }

        alreadyGiven.add(assign);

        final ReplicaID replicaID = replicaBox.getReplicaID();
        replicaTimer.add(replicaID, replicaDeadline());
        replicaMap.put(replicaID, new Replica2(replicaBox, worker));
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
        final Replica2 replica = replicaMap.get(replicaID);
        if(replica == null){
            throw new IllegalStateException("Error: Replica was not found!");
        }
        return replica.getReplicaBox().getResultKey();
    }

    /**
     * This replica didn't get any answer within given time limit. Create another one.
     * Doesn't have to report worker, he might still come up with an answer.
     *
     * If the replica was returned before this is called or if the replicaID doesn't exist, the state is unchanged.
     *
     * @param replicaID Replica that was outdated
     */
    public synchronized void replicaOutdated(ReplicaID replicaID){
        //TODO validate now or wait?

//        Replica oldReplica = replicaMap.get(replicaID);
//        if(oldReplica==null){
//            //It might already be returned! Hence not present in replicaMap
//            return;
//            //throw new IllegalArgumentException("ReplicaID "+replicaID+" doesn't exist so it cannot be outdated!");
//        }
//        Random random = new Random();
//        Replica replica = new Replica(oldReplica.getReplicaBox().getTaskMeta());
//
//        while(replicaMap.containsKey(replica.getReplicaBox().getReplicaID())){
//            replica = new Replica(oldReplica.getReplicaBox().getTaskMeta());
//        }
//        replicaMap.put(replica.getReplicaBox().getReplicaID(), replica);
//        stagedReplicas.addFirst(replica);
    }
    public synchronized void replicaFailed(ReplicaID replicaID){
        //TODO
    }

    /**
     *
     * @param replicaID ID of a replica
     * @param result Computed result of the replica
     */
    public synchronized void replicaFinished(ReplicaID replicaID, byte[] result){
        if(result == null){
            throw new IllegalArgumentException("Error: don't give null result!");
        }

        TaskData taskData = taskDataMap.get(replicaID);
        if(taskData == null){
            throw new IllegalStateException("Couldn't find TaskData in taskDataMap!");
        }
//        if(! assignedTasks.get(worker).contains(taskData)){
//            throw new IllegalStateException("This TaskData was not found for this worker in assignedTasks!");
//        }

        TaskResultData resultData = resultDataMap.get(taskData.taskID());
        if(! resultData.pendingReplicas.remove(replicaID)){
            throw new IllegalStateException("Expected replicaID to be in pendingReplicas!");
        }
        resultData.returnedReplicas.put(replicaID, result);

        //TODO decision on wait or not
        //TODO handle latecomer
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
        Replica2 replica = replicaMap.get(replicaID);
        return replica != null && replica.getWorker().equals(workerID);
    }

//    public synchronized void replicaFailed(ReplicaID replicaID){
//        //TODO implement: note failure as a result. Make comparison later.
//    }
//
//    public synchronized Collection<Replica2> pendingReplicas(){
//        //TODO implement? Want to download results if there have come any to DHT while this job owner was offline
//        //TODO Actually this would be better handled by ReplicaTimer2... Adding this to Sprint log
//        return null;
//    }

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

    private class ReplicaTimer2 implements Serializable{

        private final long UPDATE_TIME;

        private final PriorityQueue<ReplicaTimeout2> queue = new PriorityQueue<>();

        /**
         * @param updateTime Number of Milliseconds between check queue
         */
        public ReplicaTimer2(long updateTime) {
            UPDATE_TIME =  updateTime;
        }

        /**
         * Clock that updates this timer. This class must be Serializable which {@link java.util.Timer} isn't.
         * @return Runnable
         */
        public Runnable createUpdater(){
            return new Runnable() {
                @Override
                public void run() {
                    Timer timer = new Timer(true);
                    timer.schedule(new TimerTask() {
                        @Override
                        public void run() {
                            update();
                        }
                    }, UPDATE_TIME/2, UPDATE_TIME);
                }
            };
        }

        /**
         *
         * @param replicaID ID of a replica
         * @param date Expiration date of the replica
         */
        public synchronized void add(ReplicaID replicaID, Date date){
            ReplicaTimeout2 replicaTimeout = new ReplicaTimeout2(replicaID, date);
            queue.add(replicaTimeout);
        }

        /**
         * Called by clock to check the queue. Requires Outdater to be set.
         */
        private synchronized void update(){
            final Date currentTime = new Date();
            if(queue.peek()==null){
//            System.out.println("ReplicaTimer2: queue empty on update");
                return;
            }
            while(queue.peek()!=null && queue.peek().getDate().compareTo(currentTime) < 0){
                ReplicaTimeout2 outdated = queue.remove();
                replicaOutdated(outdated.getReplicaID());
            }
//        long timeDiff = queue.peek().getDate().getTime()-currentTime.getTime();
//        System.out.println("TimeDiff to next element: "+timeDiff);
        }


        private class ReplicaTimeout2 implements Serializable, Comparable<ReplicaTimeout2>{

            private final Date date;
            private final ReplicaID replicaID;

            private ReplicaTimeout2(ReplicaID replicaID, Date date) {
                this.date = date;
                this.replicaID = replicaID;
            }

            public ReplicaID getReplicaID() {
                return replicaID;
            }

            public Date getDate() {
                return date;
            }

            /**
             *
             * @param replicaTimeout Other ReplicaTimer2
             * @return comparison
             */
            @Override
            public int compareTo(ReplicaTimeout2 replicaTimeout) {
                if(replicaTimeout==null){
                    return 1;
                }
                return date.compareTo(replicaTimeout.date);
            }
        }
    }
}
