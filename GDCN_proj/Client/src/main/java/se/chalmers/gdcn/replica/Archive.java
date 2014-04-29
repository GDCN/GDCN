package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.replica.ReplicaManager.TaskID;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;
import se.chalmers.gdcn.utils.ByteArray;

import java.io.File;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Created by Leif on 2014-04-15.
 *
 * Future unimplemented class that can store validated results
 */
public class Archive implements Serializable {

    private final Map<TaskID, CanonicalResult> resultMap = new HashMap<>();

    //TODO implement

    public void archiveResult(TaskData taskData, ByteArray data, double quality, Set<WorkerID> advocatingWorkers) {
        PathManager pathManager = PathManager.jobOwner(taskData.getJobName());
        File location = new File(pathManager.getCanonicalResultFilePath(taskData.getTaskMeta().getTaskName()));
        resultMap.put(taskData.taskID(), new CanonicalResult(data, quality, advocatingWorkers, location));
    }

    public CanonicalResult getArchivedResult(TaskID taskID) {
        return resultMap.get(taskID);
    }
}
