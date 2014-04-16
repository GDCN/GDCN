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
public class ReplicaManager implements Serializable{

    private final int REPLICAS;
    private final int EXPECTED_REPUTATION;

    private final int CALENDAR_FIELD;
    private final int CALENDAR_VALUE;

    private final WorkerNodeManager workerNodeManager;
    private final ReplicaTimer2 replicaTimer;

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
    private static class TaskResultData{
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
     * @return ReplicaOLD info if there are any. Returns null if queue is empty.
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
        replicaMap.put(replicaID, new Replica(replicaBox, worker));
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
        if(! resultData.pendingReplicas.remove(replicaID)){
            throw new IllegalStateException("Expected replicaID to be in pendingReplicas!");
        }
        return resultData;
    }

    /**
     * This replica didn't get any answer within given time limit. Create another one.
     * Doesn't have to report worker, he might still come up with an answer.
     *
     * If the replica was returned before this is called or if the replicaID doesn't exist, the state is unchanged.
     *
     * @param replicaID ReplicaOLD that was outdated
     */
    public synchronized void replicaOutdated(ReplicaID replicaID){
        TaskResultData resultData = returned(replicaID);
        resultData.outdatedReplicas.add(replicaID);
        //TODO validate now or wait?
    }

    public synchronized void replicaFailed(ReplicaID replicaID){
        TaskResultData resultData = returned(replicaID);
        resultData.failedReplicas.add(replicaID);
        //TODO validate now or wait?
    }

    public synchronized void replicaFinished(ReplicaID replicaID, byte[] result){
        if(result == null){
            throw new IllegalArgumentException("Error: don't give null result!");
        }
        TaskResultData resultData = returned(replicaID);
        resultData.returnedReplicas.put(replicaID, result);

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


    public synchronized Collection<Replica> pendingReplicas(){
        //TODO implement if want this, easy to do now
        //TODO would be better handled by ReplicaTimer2 ?
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