package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.replica.ReplicaManager.TaskID;

import java.io.Serializable;

/**
 * Created by HalfLeif on 2014-04-15.
 */
public class TaskData implements TaskCompare, Serializable{
    private final TaskMeta taskMeta;
    private final String jobName;

    private int replicasLeft;
    private float reputationNeeded;

    public TaskData(TaskMeta taskMeta, String jobName, int replicas, float reputationNeeded) {
        this.taskMeta = taskMeta;
        this.jobName = jobName;
        this.replicasLeft = replicas;
        this.reputationNeeded = reputationNeeded;
    }

    public TaskMeta giveTask(float reputation){
        replicasLeft--;
        reputationNeeded-=reputation;
        return taskMeta;
    }

    public boolean enoughGiven(){
        return replicasLeft <=0 && reputationNeeded <= 0;
    }

    public String getJobName() {
        return jobName;
    }

    public TaskID taskID(){
        return new TaskID(jobName + taskMeta.getTaskName());
    }

    @Override
    public float value(){
        //TODO replicasLeft can get negative as it is now...
        if(reputationNeeded == 0){
            return 0;
        }
        if(replicasLeft == 0){
            return Float.MAX_VALUE;
        }
        return reputationNeeded/replicasLeft;
    }
}
