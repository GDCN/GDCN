package files;

import command.communicationToUI.CommandWord;
import command.communicationToUI.NetworkInterface;
import net.tomp2p.storage.Data;
import replica.ReplicaManager;
import taskbuilder.communicationToClient.TaskFailureListener;
import taskbuilder.fileManagement.PathManager;

import java.io.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by HalfLeif on 2014-03-05.
 */
public class JobUploader extends AbstractFileMaster{

    private JobUploader(PathManager pathManager, TaskMeta taskMeta, NetworkInterface client, TaskFailureListener taskFailureListener) throws TaskMetaDataException {
        super(taskMeta, client, taskFailureListener, CommandWord.PUT, pathManager);
    }

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
            FileDep fileDep = new FileDep(task, "tasks", task, true, 0);
            dependencyTasks.add(fileDep);
//            allFileDependencies.add(fileDep); //Since taskMeta shall be sent using MPI instead...
        }

        List<TaskMeta> taskMetas = new ArrayList<>();
        for(FileDep fileDep : dependencyTasks){
            TaskMeta taskMeta = AbstractFileMaster.readMetaFile( AbstractFileMaster.pathTo(manager, fileDep));
            taskMetas.add(taskMeta);
            allFileDependencies.add(taskMeta.getModule());
            allFileDependencies.addAll(taskMeta.getDependencies());
        }
        replicaManager.loadTasksAndReplicate(taskMetas);

        for(FileDep fileDep : allFileDependencies){
            System.out.println(" Dependency: "+fileDep.getFileName());
        }

        TaskMeta totalJobMeta = new TaskMeta("Upload"+jobName, null, new ArrayList<>(allFileDependencies));

        return new JobUploader(manager, totalJobMeta, client, taskFailureListener);
    }

    @Override
    protected void ifFileExist(FileDep fileDep) {
        File file = super.pathTo(fileDep);

        try {
            System.out.println("Put " + pathTo(fileDep));
            client.put(fileDep.getDhtKey(), new Data(FileUtils.fromFile(file)));
        } catch (IOException e) {
            e.printStackTrace();
            //TODO better output?
            System.out.println("Failed to put " + pathTo(fileDep) + "\n"+e.getMessage());
        }
    }

    @Override
    protected void ifFileDoNotExist(FileDep fileDep) {
        //TODO better output?
        System.out.println("Didn't find file " + pathTo(fileDep));
        //TODO fail task and abort
    }

    @Override
    protected void operationForDependentFileSuccess(FileDep fileDep, Object result) {
        System.out.println("Successfully put " + fileDep.getFileName());
        super.fileDependencyResolved(fileDep);
    }


}
