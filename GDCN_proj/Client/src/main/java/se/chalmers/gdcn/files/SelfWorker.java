package se.chalmers.gdcn.files;

import se.chalmers.gdcn.taskbuilder.Task;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;

/**
 * Created by Leif on 2014-04-21.
 */
public class SelfWorker {

    private final FileSolver fileSolver;
    private final String taskName;

    public SelfWorker(TaskMeta taskMeta, String jobName) throws TaskMetaDataException {
        fileSolver = new FileSolver(taskMeta, jobName);
        taskName = taskMeta.getTaskName();
    }

    public String futureResultFilePath(){
        return fileSolver.pathManager.getResultFilePath(taskName);
    }

    public Task workSelf(TaskMeta taskMeta, TaskListener listener) throws TaskMetaDataException {
        return new Task(fileSolver.pathManager.getProjectName(), taskMeta.getTaskName(), fileSolver.getModuleName(), fileSolver.getResourceFiles(), listener);
    }

    private static class FileSolver extends AbstractFileMaster{

        public FileSolver(TaskMeta taskMeta, String jobName) throws TaskMetaDataException {
            super(taskMeta, null, null, null, PathManager.jobOwner(jobName));
        }

        @Override
        protected void ifFileExist(FileDep fileDep) {

        }

        @Override
        protected void ifFileDoNotExist(FileDep fileDep) {

        }

        @Override
        protected void operationForDependentFileSuccess(FileDep fileDep, Object result) {

        }
    }
}
