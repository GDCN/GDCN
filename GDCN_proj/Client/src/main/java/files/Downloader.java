package files;

import command.communicationToUI.CommandWord;
import command.communicationToUI.NetworkInterface;
import command.communicationToUI.OperationFinishedEvent;
import net.tomp2p.storage.Data;
import taskbuilder.Task;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.PathManager;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.*;
import java.util.concurrent.Semaphore;

/**
 * Created by HalfLeif on 2014-03-05.
 */
public class Downloader extends AbstractFileMaster {

    private Downloader(PathManager pathManager, TaskMeta taskMeta, NetworkInterface client, TaskListener taskListener) throws TaskMetaDataException {
        super(taskMeta, client, taskListener, CommandWord.GET, pathManager);
    }

    public static Downloader create(String projectName, String taskName, NetworkInterface client, TaskListener taskListener) throws TaskMetaDataException {

        PathManager manager = PathManager.worker(projectName);
        TaskMeta taskMeta = resolveMetaFile(taskName, client, taskListener, manager);

        return new Downloader(manager, taskMeta, client, taskListener);
    }

    private static TaskMeta resolveMetaFile(String taskName, NetworkInterface client, final TaskListener taskListener, PathManager pathManager) throws TaskMetaDataException {
        final File file = new File(pathManager.taskMetaDir() + taskName + ".json");
        if(file.exists()){
            System.out.println("Downloader: YAY file exist!");
            try {
                return AbstractFileMaster.readMetaFile(file);
            } catch (FileNotFoundException e) {
                e.printStackTrace();
                throw new TaskMetaDataException("Error reading file: "+file.getAbsolutePath());
            }
        }

        final String key = taskName+".json";
        final Semaphore operationFinished = new Semaphore(0);

        PropertyChangeListener localListener = new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                if(! (evt instanceof OperationFinishedEvent)){
                    return;
                }
                OperationFinishedEvent event = (OperationFinishedEvent) evt;
                if(event.getCommandWord() != CommandWord.GET){
                    return;
                }
                if(event.getOperation().getKey() != key){
                    return;
                }
                if(event.getOperation().isSuccess()){
                    Data data = (Data) event.getOperation().getResult();
                    toFile(file, data.getData());
                } else {
                    System.out.println("WARNING: "+key+" wasn't found. Fail");
                }
                operationFinished.release();
            }
        };
        client.addListener(localListener);

        client.get(key);
        operationFinished.acquireUninterruptibly();
        client.removeListener(localListener);

        if(file.exists()){
            System.out.println("Downloader: YAY file exist after download!");
            try {
                return AbstractFileMaster.readMetaFile(file);
            } catch (FileNotFoundException e) {
                e.printStackTrace();
                throw new TaskMetaDataException("Error reading file: "+file.getAbsolutePath());
            }
        } else {
            throw new TaskMetaDataException("Unable to resolve key: "+key);
        }
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
        client.get(fileDep.getDhtKey());
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
        return taskMeta.getModule().getFileName().replace(".hs", "");
    }

    /**
     * Build new Task specified by the meta-file that was parsed earlier.
     * @param listener Listener for success on task
     * @return Task object
     */
    public Task buildTask(TaskListener listener){
        return new Task(pathManager.getProjectName(), taskMeta.getTaskName(), getModuleName(), getResourceFiles(), listener);
    }


    /**
     * Outputs some arbitrary data to file
     * @param file File to turn into byte[]
     * @param data contents of file
     */
    private static void toFile(File file, byte[] data){
        System.out.println("Attempt create file "+file.getAbsolutePath());
        File parent = file.getParentFile();
        parent.mkdirs();

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
