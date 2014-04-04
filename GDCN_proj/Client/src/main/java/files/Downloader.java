package files;

import command.communicationToUI.CommandWord;
import command.communicationToUI.NetworkInterface;
import net.tomp2p.storage.Data;
import taskbuilder.Task;
import taskbuilder.communicationToClient.TaskFailureListener;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.PathManager;

import java.io.*;

/**
 * Created by HalfLeif on 2014-03-05.
 */
public class Downloader extends AbstractFileMaster {

    public Downloader(TaskMeta taskMeta, String projectName, NetworkInterface client, TaskFailureListener taskFailureListener) throws TaskMetaDataException {
        super(taskMeta, client, taskFailureListener, CommandWord.GET, PathManager.worker(projectName));
    }


    /**
     * {@inheritDoc}
     */
    @Override
    protected void ifFileExist(FileDep fileDep) {
        System.out.println("Found file :D - " + pathTo(fileDep));
        super.fileDependencyResolved(fileDep);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void ifFileDoNotExist(FileDep fileDep) {
        //TODO better output?
        System.out.println("Didn't find file " + pathTo(fileDep));
        client.get(fileDep.getDhtKey());
        //Handling OperationFinished is done in AbstractFileMaster
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void operationForDependentFileSuccess(FileDep fileDep, Object result) {
        File file = pathTo(fileDep);
        Data data = (Data) result;
        FileUtils.toFile(file, data.getData());
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
     * @deprecated
     * TODO remove this method if not needed...
     *
     * @param taskName Name of task
     * @param client Client
     * @param taskListener Listener
     * @param pathManager Pathmanager
     * @return TaskMeta
     * @throws TaskMetaDataException
     */
//    private static TaskMeta resolveMetaFile(String taskName, NetworkInterface client, final TaskListener taskListener, PathManager pathManager) throws TaskMetaDataException {
//        final File file = new File(pathManager.taskMetaDir() + taskName + ".json");
//        if(file.exists()){
//            System.out.println("Downloader: YAY file exist!");
//            try {
//                return AbstractFileMaster.readMetaFile(file);
//            } catch (FileNotFoundException e) {
//                e.printStackTrace();
//                throw new TaskMetaDataException("Error reading file: "+file.getAbsolutePath());
//            }
//        }
//
//        final String key = taskName+".json";
//        final Semaphore operationFinished = new Semaphore(0);
//
//        PropertyChangeListener localListener = new PropertyChangeListener() {
//            @Override
//            public void propertyChange(PropertyChangeEvent evt) {
//                if(! (evt instanceof OperationFinishedEvent)){
//                    return;
//                }
//                OperationFinishedEvent event = (OperationFinishedEvent) evt;
//                if(event.getCommandWord() != CommandWord.GET){
//                    return;
//                }
//                if(! event.getOperation().getKey().equals(key) ){
//                    return;
//                }
//                if(event.getOperation().isSuccess()){
//                    Data data = (Data) event.getOperation().getResult();
//                    toFile(file, data.getData());
//                } else {
//                    System.out.println("WARNING: "+key+" wasn't found. Fail");
//                }
//                operationFinished.release();
//            }
//        };
//        client.addListener(localListener);
//
//        client.get(key);
//        operationFinished.acquireUninterruptibly();
//        client.removeListener(localListener);
//
//        if(file.exists()){
//            System.out.println("Downloader: YAY file exist after download!");
//            try {
//                return AbstractFileMaster.readMetaFile(file);
//            } catch (FileNotFoundException e) {
//                e.printStackTrace();
//                throw new TaskMetaDataException("Error reading file: "+file.getAbsolutePath());
//            }
//        } else {
//            throw new TaskMetaDataException("Unable to resolve key: "+key);
//        }
//    }

}
