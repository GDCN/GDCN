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
    private final PathManager jobPathManager;
    private final PathManager workerPathManager;

    public SelfWorker(TaskMeta taskMeta, String jobName) throws TaskMetaDataException {
        taskName = taskMeta.getTaskName();
        jobPathManager = PathManager.jobOwner(jobName);
        workerPathManager = PathManager.worker(jobName);
    }

    /**
     * Returns absolute path to result file from this self work.
     * This file will be in the /data directory.
     * @return Absolute path
     */
    public String futureResultFilePath(){
//        return jobPathManager.getResultFilePath(taskName);
        return workerPathManager.getResultFilePath(taskName);
    }

    /**
     * Creates a Task object for the local job owner to work on himself.
     *
     * @param taskMeta Meta info of this replica
     * @param listener Listener for success or failure
     * @return Runnable Task
     * @throws TaskMetaDataException
     */
    public Task workSelf(TaskMeta taskMeta, TaskListener listener) throws TaskMetaDataException {
        try {
            copyFiles();
        } catch (IOException e) {
            e.printStackTrace();
            listener.taskFailed(taskName, e.getMessage());
        }

        return new Task(jobPathManager.getProjectName(), taskMeta.getTaskName(), FileManagementUtils.moduleName(taskMeta),
                FileManagementUtils.getResourceFiles(jobPathManager, taskMeta), listener);
    }

    /**
     * Copies files from jobs/ to data/ folder in order to perform local work.
     * @throws IOException
     */
    private void copyFiles() throws IOException {
//        PathManager workerPathManager = PathManager.worker(jobPathManager.getProjectName());
        FileUtils.copyDirectory( new File(jobPathManager.taskResourcesDir()), new File(workerPathManager.taskResourcesDir()));
        FileUtils.copyDirectory( new File(jobPathManager.taskCodeDir()), new File(workerPathManager.taskCodeDir()));
    }
}
