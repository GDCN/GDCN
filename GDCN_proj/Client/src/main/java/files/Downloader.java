package files;

import command.communicationToUI.ClientInterface;
import command.communicationToUI.CommandWord;
import net.tomp2p.storage.Data;
import taskbuilder.Task;
import taskbuilder.communicationToClient.TaskListener;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by HalfLeif on 2014-03-05.
 */
public class Downloader extends AbstractFileMaster {

    private final String taskName;

    /**
     * Creates FileMaster object that reads meta-file for a task. Run {@link FileMaster#runAndAwait()} for
     * solving the dependencies.
     *
     * @param projectName  Name of project
     * @param taskNames     Name of task
     * @param client       Client for downloading files from network (DHT)
     * @param taskListener Listener to learn about failures such as unresolved dependencies.
     * @throws java.io.FileNotFoundException if meta-file is not found. Path to search on is derived from projectName and taskName.
     */
    private Downloader(String projectName, List<String> taskNames, ClientInterface client, TaskListener taskListener) throws FileNotFoundException, TaskMetaDataException {
        super(projectName, taskNames, client, taskListener, CommandWord.GET);
        this.taskName = taskNames.get(0);
    }

    public static Downloader create(String projectName, String taskName, ClientInterface client, TaskListener taskListener) throws FileNotFoundException, TaskMetaDataException {
        List<String> taskNames = new ArrayList<>();
        taskNames.add(taskName);
        return new Downloader(projectName, taskNames, client, taskListener);
    }

    @Override
    protected void ifFileExist(FileDep fileDep) {
        System.out.println("Found file :D - " + pathTo(fileDep));
        super.fileDependencyResolved(fileDep);
    }

    @Override
    protected void ifFileDoNotExist(FileDep fileDep) {
        //TODO better output?
        System.out.println("Didn't find file " + pathTo(fileDep));
        client.get(fileDep.getKey());
    }

    @Override
    protected void operationForDependentFileSuccess(FileDep fileDep, Object result) {
        File file = pathTo(fileDep);
        Data data = (Data) result;
        toFile(file, data.getData());
    }

    /**
     *
     * @return Name of haskell module this taskmeta uses
     */
    private String getModuleName(){
        return taskMetas.get(taskName).getModule().getFileName().replace(".hs", "");
    }

    /**
     * Build new Task specified by the meta-file that was parsed earlier.
     * @param listener Listener for success on task
     * @return Task object
     */
    public Task buildTask(TaskListener listener){
        return new Task(pathManager.getProjectName(), taskName, getModuleName(), getResourceFiles(taskName), listener);
    }


    /**
     * Outputs some arbitrary data to file
     * @param file File to turn into byte[]
     * @param data contents of file
     */
    private static void toFile(File file, byte[] data){
        //TODO use Box class and do checksum
        BufferedOutputStream outputStream = null;
        try {
            outputStream = new BufferedOutputStream(new FileOutputStream(file));
            outputStream.write(data);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (outputStream != null) {
                try {
                    outputStream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
