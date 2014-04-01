package files;

import command.communicationToUI.CommandWord;
import command.communicationToUI.NetworkInterface;
import net.tomp2p.storage.Data;
import replica.ReplicaManager;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.PathManager;

import java.io.*;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by HalfLeif on 2014-03-05.
 */
public class Uploader extends AbstractFileMaster{

    private Uploader(PathManager pathManager, TaskMeta taskMeta, NetworkInterface client, TaskListener taskListener) throws TaskMetaDataException {
        super(taskMeta, client, taskListener, CommandWord.PUT, pathManager);
    }

    public static Uploader create(String jobName, NetworkInterface client, TaskListener taskListener, ReplicaManager replicaManager) throws FileNotFoundException, TaskMetaDataException {

        PathManager manager = PathManager.jobOwner(jobName);
        File file = new File(manager.taskMetaDir());

        System.out.println("MetaDir: "+file.getAbsolutePath());

        String[] tasks = file.list();

        Set<FileDep> allFileDependencies = new HashSet<>();
        List<FileDep> dependencyTasks = new ArrayList<>();
        for(String task:tasks){
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

        TaskMeta totalJobMeta = new TaskMeta(jobName, "Upload"+jobName, null, new ArrayList<>(allFileDependencies));

        return new Uploader(manager, totalJobMeta, client, taskListener);
    }

    @Override
    protected void ifFileExist(FileDep fileDep) {
        File file = super.pathTo(fileDep);

        try {
            System.out.println("Put " + pathTo(fileDep));
            client.put(fileDep.getDhtKey(), new Data(fromFile(file)));
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

    public static byte[] fromFile(File file) throws IOException {

        InputStream inputStream = null;

        try {
            //TODO Possibly use MD5 instead, if SHA-1 is too slow
            MessageDigest digest = MessageDigest.getInstance("SHA-1");

            inputStream = new BufferedInputStream(new FileInputStream(file));
            DigestInputStream digestInputStream = new DigestInputStream(inputStream, digest);

            byte[] data = new byte[(int) file.length()];

            //TODO actually use digest
            digestInputStream.read(data);
            byte[] digestArray = digest.digest();

            return data;

        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } finally {
            if (inputStream != null) {
                inputStream.close();
            }
        }
        return null;
    }



}
