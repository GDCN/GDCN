package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.files.TaskMeta;

/**
 * Created by HalfLeif on 2014-04-15.
 */
public class TaskData implements Comparable<TaskData>{
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

    private float value(){
        return reputationNeeded/replicasLeft;
    }

    @Override
    public int compareTo(TaskData o) {
        if(value()>o.value()){
            return 1;
        } else if(value()<o.value()){
            return -1;
        } else{
            return 0;
        }
    }
}
