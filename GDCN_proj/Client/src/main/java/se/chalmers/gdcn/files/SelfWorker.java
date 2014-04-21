package se.chalmers.gdcn.files;

import se.chalmers.gdcn.taskbuilder.Task;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;

/**
 * Created by Leif on 2014-04-21.
 */
public class SelfWorker {

    public static Runnable workSelf(TaskMeta taskMeta, String jobName, TaskListener listener) throws TaskMetaDataException {

        FileSolver fileSolver = new FileSolver(taskMeta, jobName);
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
