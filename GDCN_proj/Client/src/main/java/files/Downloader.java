package files;

import command.communicationToUI.ClientInterface;
import command.communicationToUI.CommandWord;
import net.tomp2p.storage.Data;
import taskbuilder.Task;
import taskbuilder.communicationToClient.TaskListener;

import java.io.*;

/**
 * Created by HalfLeif on 2014-03-05.
 */
public class Downloader extends AbstractFileMaster {
    /**
     * Creates FileMaster object that reads meta-file for a task. Run {@link FileMaster#runAndAwait()} for
     * solving the dependencies.
     *
     * @param projectName  Name of project
     * @param taskName     Name of task
     * @param client       Client for downloading files from network (DHT)
     * @param taskListener Listener to learn about failures such as unresolved dependencies.
     * @throws java.io.FileNotFoundException if meta-file is not found. Path to search on is derived from projectName and taskName.
     */
    public Downloader(String projectName, String taskName, ClientInterface client, TaskListener taskListener) throws FileNotFoundException, TaskMetaDataException {
        super(projectName, taskName, client, taskListener, CommandWord.GET);
    }

    @Override
    protected void ifFileExist(FileDep fileDep) {
        System.out.println("Found file :D - " + pathTo(fileDep));
        super.fileDependencyResolved(fileDep);
    }

    @Override
    protected void ifFileDoNotExist(FileDep fileDep) {
        System.out.println("Didn't find file " + pathTo(fileDep));
        client.get(fileDep.getKey());
    }

    @Override
    protected void operationForDependentFileSuccess(FileDep fileDep, Object result) {
        //TODO do checksum?
        File file = pathTo(fileDep);
        Data data = (Data) result;
        toFile(file, ((Data) result).getData());
    }

    /**
     *
     * @return Name of haskell module this taskmeta uses
     */
    private String getModuleName(){
        return taskMeta.getModule().getFileName().replace(".hs", "");
    }

    /**
     * Build new Task specified by the meta-file that was parsed earlier.
     * @param listener Listener for success on task
     * @return
     */
    public Task buildTask(TaskListener listener){
        return new Task(pathManager.getProjectName(), taskName, getModuleName(), getResourceFiles(), listener);
    }


    /**
     * Outputs some arbitrary data to file
     * @param file
     * @param data
     */
    private static void toFile(File file, byte[] data){
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
