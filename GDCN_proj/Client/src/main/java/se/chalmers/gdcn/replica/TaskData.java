package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.files.TaskMeta;

import java.io.Serializable;

/**
 * Created by HalfLeif on 2014-04-15.
 */
public class TaskData implements TaskCompare, Serializable{
    private final TaskMeta taskMeta;

    private int replicasLeft;
    private float reputationNeeded;

    public TaskData(TaskMeta taskMeta, int replicas, float reputationNeeded) {
        this.taskMeta = taskMeta;
        this.replicasLeft = replicas;
        this.reputationNeeded = reputationNeeded;
    }

    public Replica giveReplica(float reputation){
        replicasLeft--;
        reputationNeeded-=reputation;
        return new Replica(taskMeta);
    }

    @Override
    public float value(){
        return reputationNeeded/replicasLeft;
    }
}
