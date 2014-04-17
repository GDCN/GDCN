package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.replica.ReplicaManager.TaskID;

import java.io.Serializable;

/**
 * Created by HalfLeif on 2014-04-15.
 */
class TaskData implements TaskCompare, Serializable{
    private final TaskMeta taskMeta;
    private final String jobName;
    private final TaskID taskID;

    private int replicasLeft;
    private float reputationNeeded;

    public TaskData(TaskMeta taskMeta, String jobName, int replicas, float reputationNeeded) {
        this.taskMeta = taskMeta;
        this.jobName = jobName;
        this.replicasLeft = replicas;
        this.reputationNeeded = reputationNeeded;
        this.taskID = new TaskID(jobName + taskMeta.getTaskName());
    }

    /**
     * Assigns worker to task depending on its reputation. Ie Smart assign
     * OBS! Changes the state of this object, if it is inside a sorted collection, it must be re-sorted!
     * @param reputation the workers reputation
     * @return task for that worker to work on
     */
    public TaskMeta giveTask(float reputation){
        replicasLeft--;
        reputationNeeded-=reputation;
        return taskMeta;
    }

    /**
     * Enough replicas has been given for this task. Depends on reputation as well.
     * @return true if this task can be validated.
     */
    public boolean enoughGiven(){
        return replicasLeft <=0 && reputationNeeded <= 0;
    }

    public TaskMeta getTaskMeta() {
        return taskMeta;
    }

    public String getJobName() {
        return jobName;
    }

    /**
     * @return id of this task
     */
    public TaskID taskID(){
        return taskID;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public float value(){
        //Remember that floor() is called before ceiling()

        if(reputationNeeded > 0 && replicasLeft > 0){
            //Most common case: give this task to a worker with appropriate reputation
            return reputationNeeded/replicasLeft;
        }

        if(reputationNeeded <=0 && replicasLeft > 0){
            //If two tasks have sufficient reputation already, work on the one with the fewest replicas left.
            return -replicasLeft;
        }

        if(reputationNeeded > 0 && replicasLeft <= 0){
            //Should be worked on by someone with high reputation
            //Want this last reputation to optimally be solved in one replica
            return reputationNeeded;
        }

        //Should be chosen as the very last task to work on
        //This can happen when there are no other tasks and some replicas was recently given.
        return Float.MAX_VALUE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String order() {
        return taskID().toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TaskData)) return false;

        TaskData taskData = (TaskData) o;

        if (!jobName.equals(taskData.jobName)) return false;
        if (!taskMeta.equals(taskData.taskMeta)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = taskMeta.hashCode();
        result = 31 * result + jobName.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "TaskData{" +
                "taskMeta=" + taskMeta +
                ", jobName='" + jobName + '\'' +
                '}';
    }
}
