package se.chalmers.gdcn.files;

import se.chalmers.gdcn.taskbuilder.Task;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;

/**
 * Created by Leif on 2014-04-21.
 */
public class SelfWorker {

//    private final FileSolver fileSolver;
    private final String taskName;
    private final PathManager pathManager;

    public SelfWorker(TaskMeta taskMeta, String jobName) throws TaskMetaDataException {
        taskName = taskMeta.getTaskName();
        pathManager = PathManager.jobOwner(jobName);
    }

    public String futureResultFilePath(){
        return pathManager.getResultFilePath(taskName);
    }

    public Task workSelf(TaskMeta taskMeta, TaskListener listener) throws TaskMetaDataException {
        return new Task(pathManager.getProjectName(), taskMeta.getTaskName(), FileUtils.moduleName(taskMeta),
                FileUtils.getResourceFiles(pathManager, taskMeta), listener);
    }
}
