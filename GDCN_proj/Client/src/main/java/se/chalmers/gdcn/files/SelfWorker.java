package se.chalmers.gdcn.files;

import org.codehaus.plexus.util.FileUtils;
import se.chalmers.gdcn.taskbuilder.Task;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;

import java.io.File;
import java.io.IOException;

/**
 * Created by Leif on 2014-04-21.
 */
public class SelfWorker {

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
        try {
            copyFiles(taskMeta);
        } catch (IOException e) {
            e.printStackTrace();
            listener.taskFailed(taskName, e.getMessage());
        }

        return new Task(pathManager.getProjectName(), taskMeta.getTaskName(), FileManagementUtils.moduleName(taskMeta),
                FileManagementUtils.getResourceFiles(pathManager, taskMeta), listener);
    }

    private void copyFiles(TaskMeta taskMeta) throws IOException {
        PathManager workPathMan = PathManager.worker(pathManager.getProjectName());
        FileUtils.copyDirectory( new File(pathManager.taskResourcesDir()), new File(workPathMan.taskResourcesDir()));
        FileUtils.copyDirectory( new File(pathManager.taskCodeDir()), new File(workPathMan.taskCodeDir()));
    }
}
