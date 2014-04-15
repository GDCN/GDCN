package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.files.TaskMeta;

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

    public Replica giveReplica(float reputation){
        replicasLeft--;
        reputationNeeded-=reputation;
        //TODO replica ID must be unique!
        return new Replica(taskMeta);
    }

    public String getJobName() {
        return jobName;
    }

    @Override
    public float value(){
        return reputationNeeded/replicasLeft;
    }
}
