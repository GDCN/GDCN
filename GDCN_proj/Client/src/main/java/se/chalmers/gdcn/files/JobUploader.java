package se.chalmers.gdcn.files;

import net.tomp2p.peers.Number160;
import net.tomp2p.storage.Data;
import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.NetworkInterface;
import se.chalmers.gdcn.replica.ReplicaManager;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskFailureListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by HalfLeif on 2014-03-05.
 */
public class JobUploader extends AbstractFileMaster{

    private final TaskFailureListener taskFailureListener;

    private JobUploader(PathManager pathManager, TaskMeta taskMeta, NetworkInterface client, TaskFailureListener taskFailureListener) throws TaskMetaDataException {
        super(taskMeta, client, taskFailureListener, CommandWord.PUT, pathManager);
        this.taskFailureListener = taskFailureListener;
    }

    /**
     *
     * @param jobName Name of directory for this job
     * @param client Client that handles network operations
     * @param taskFailureListener Listener that notifies on failures
     * @param replicaManager ReplicaManager that makes replicas of read tasks
     * @return JobUploader that pushes all job files to DHT
     *
     * @throws FileNotFoundException
     * @throws TaskMetaDataException
     */
    public static JobUploader create(String jobName, NetworkInterface client, TaskFailureListener taskFailureListener, ReplicaManager replicaManager) throws FileNotFoundException, TaskMetaDataException {

        PathManager manager = PathManager.jobOwner(jobName);
        File file = new File(manager.taskMetaDir());

        System.out.println("MetaDir: "+file.getAbsolutePath());

        String[] tasks = file.list();

        Set<FileDep> allFileDependencies = new HashSet<>();
        List<FileDep> dependencyTasks = new ArrayList<>();
        for(String task:tasks){
            if(task.equals(".DS_Store")){
                continue;
            }
            System.out.println("\t"+task);
            FileDep fileDep = new FileDep(task, "tasks", Number160.createHash(task), true, 0);
            dependencyTasks.add(fileDep);
//            allFileDependencies.add(fileDep); //Since taskMeta shall be sent using MPI instead...
        }

        List<TaskMeta> taskMetas = new ArrayList<>();


        for(FileDep fileDep : dependencyTasks){
            TaskMeta taskMeta = AbstractFileMaster.readMetaFile( FileManagementUtils.pathTo(manager, fileDep));
            taskMetas.add(taskMeta);

            taskMeta.getModule().setDhtKey(Number160.createHash(taskMeta.getModule().getFileName()));

            setFileDepDHTKey(taskMeta.getModule());

            for(FileDep f : taskMeta.getDependencies()) {
                setFileDepDHTKey(f);
            }

            allFileDependencies.add(taskMeta.getModule());
            allFileDependencies.addAll(taskMeta.getDependencies());
        }

        replicaManager.loadTasksAndReplicate(jobName, taskMetas);

        for(FileDep fileDep : allFileDependencies){
            System.out.println("  Dependency: "+fileDep.getFileName());
        }

        TaskMeta totalJobMeta = new TaskMeta("Upload"+jobName, null, new ArrayList<>(allFileDependencies));

        return new JobUploader(manager, totalJobMeta, client, taskFailureListener);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void ifFileExist(FileDep fileDep) {
        File file = FileManagementUtils.pathTo(pathManager, fileDep);

        try {
//            System.out.println("Put " + FileManagementUtils.pathTo(pathManager, fileDep));
            Data data = new Data(FileManagementUtils.fromFile(file));
            client.put(fileDep.getDhtKey(), client.getID(), data);
            //Handling OperationFinished is done in AbstractFileMaster

        } catch (IOException e) {
            e.printStackTrace();
            taskFailureListener.taskFailed(taskMeta.getTaskName(), "Failed to serialize: " + FileManagementUtils.pathTo(pathManager, fileDep) +
                    "\n"+e.getMessage());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void ifFileDoNotExist(FileDep fileDep) {
        taskFailureListener.taskFailed(taskMeta.getTaskName(), "Unable to resolve "+ FileManagementUtils.pathTo(pathManager, fileDep));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void operationForDependentFileSuccess(FileDep fileDep, Object result) {
        System.out.println("Successfully put " + fileDep.getFileName());
        super.fileDependencyResolved(fileDep);
    }

    private static void setFileDepDHTKey(FileDep f) {
        f.setDhtKey(Number160.createHash(f.getFileName()));
    }


}
