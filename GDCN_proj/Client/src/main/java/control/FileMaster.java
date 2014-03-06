package control;

import com.google.gson.Gson;
import command.communicationToUI.ClientInterface;
import command.communicationToUI.CommandWord;
import command.communicationToUI.OperationFinishedEvent;
import net.tomp2p.storage.Data;
import taskbuilder.Task;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.PathManager;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by HalfLeif on 2014-03-04.
 *
 * Class for resolving task file dependencies.
 * Checks if necessary files exist in file system, otherwise attempts to download them from DHT.
 *
 * Uses TaskListener to report error information.
 * Use {@link FileMaster#await()} to see when finished.
 */
public class FileMaster{

    private final TaskMeta taskMeta;
    private final PathManager pathManager;
    private final ClientInterface client;
    private final TaskListener taskListener;

    private final String taskName;

    private final Lock lock = new ReentrantLock();
    private final Condition allDependenciesComplete = lock.newCondition();
    private final Map<String, FileDep> unresolvedFiles = new HashMap<String, FileDep>();

    private boolean operationFailed = false;
    private boolean stillStartingUp = true;

    private final PropertyChangeListener propertyListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            if (evt instanceof OperationFinishedEvent){
                operationReturned((OperationFinishedEvent) evt);
            }
        }
    };

    /**
     * Creates FileMaster object that reads meta-file for a task. Run {@link control.FileMaster#runAndAwait()} for
     * solving the dependencies.
     *
     * @param projectName Name of project
     * @param taskName Name of task
     * @param client Client for downloading files from network (DHT)
     * @param taskListener Listener to learn about failures such as unresolved dependencies.
     * @throws FileNotFoundException if meta-file is not found. Path to search on is derived from projectName and taskName.
     */
    public FileMaster(String projectName, String taskName, ClientInterface client, TaskListener taskListener) throws FileNotFoundException, TaskMetaDataException {
        this.client = client;
        this.taskListener = taskListener;
        client.addListener(propertyListener);

        this.taskName = taskName;
        pathManager = new PathManager(projectName);
        File metaTaskFile = new File(pathManager.taskMetaDir()+getMetaFileName(taskName));

        taskMeta = readMetaFile(metaTaskFile);

        if(! taskName.equals(taskMeta.taskName)){
            throw new TaskMetaDataException("Must be error in metaFile: taskName doesn't conform with filename!");
        }
    }

    /**
     * Attempts to resolve the dependencies found in meta-file.
     */
    public void run() throws TaskMetaDataException {
        if(taskMeta != null){
            resolveDependencies();
        } else {
            throw new TaskMetaDataException("Meta data file wasn't found (or parsed correctly)!");
        }
    }

    /**
     * Just runs {@link control.FileMaster#run()} and {@link control.FileMaster#await()}
     * @return result of {@link control.FileMaster#await()}
     */
    public boolean runAndAwait() throws TaskMetaDataException {
        run();
        return await();
    }

    /**
     * Blocks current thread until all dependencies for the specified task are resolved.
     *
     * @return true if file has been properly downloaded, false if one of the dependencies couldn't be resolved.
     */
    public boolean await(){
        if(operationFailed){
            // This code is necessary to avoid deadlock if await() is called after a GET operation has failed
            // since there is no guarantee for another signal (it might have been the last file to be resolved)
            return false;
        }

        while(stillStartingUp || unresolvedFiles.size()>0){
            try {
                lock.lock();
                allDependenciesComplete.await();
                System.out.println("Got signal dependencies complete");
                lock.unlock();
            } catch (InterruptedException e) {
                System.out.println("Caught interruption: "+e.getMessage());
                continue;
            }
            if(operationFailed){
                return false;
            }
            System.out.println("Test monitor condition before exit...");
        }

        //Alternatively, ignore await() model and use TaskListener instead...
        return true;
    }

    /**
     * Build new Task specified by the meta-file that was parsed earlier.
     * @param listener Listener for success on task
     * @return
     */
    public Task buildTask(TaskListener listener){
        return new Task(pathManager.getProjectName(), taskName, getModuleName(), getResourceFiles(), listener);
    }

    private List<String> getResourceFiles() {
        List<String> resources = new ArrayList<String>();

        for(FileDep fileDep : taskMeta.dependencies){
            resources.add(pathTo(fileDep).getAbsolutePath());
        }

        return resources;
    }

    //TODO make this more safe/general
    private String getModuleName(){
        return taskMeta.module.fileName.replace(".hs","");
    }

    /**
     * Outputs some arbitrary data to file
     * @param file
     * @param data
     */
    public static void toFile(File file, byte[] data){
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


    /**
     * Parses file for MetaData of Task
     *
     * @param file Path to meta data file
     * @return Representation of meta data content
     * @throws FileNotFoundException if file isn't found
     */
    private TaskMeta readMetaFile(File file) throws FileNotFoundException {

        Reader reader = null;
        try {
            reader = new InputStreamReader(new BufferedInputStream(new FileInputStream(file)));

            Gson gson = new Gson();
            return gson.fromJson(reader, TaskMeta.class);

        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    private File pathTo(FileDep fileDep){
        return new File(pathManager.projectDir() + fileDep.location + File.separator + fileDep.fileName);
    }

    private void resolveDependencies() throws TaskMetaDataException {
        unresolvedFiles.clear();

        List<FileDep> deps = new ArrayList<FileDep>(taskMeta.dependencies);
        deps.add(taskMeta.module);

        for(FileDep fileDep : deps){
            File file = pathTo(fileDep);
            if(file.exists()){
                if(file.isDirectory()){
                    throw new TaskMetaDataException("Files in dependencies should not be directories! File: "
                            +file+" for task "+taskMeta.taskName);
                }
                System.out.println("Found file :D - " + file.toString());
                //TODO checksum?
            } else {
                //TODO remove print
                System.out.println("Didn't find file " + file.toString());

                lock.lock();
                unresolvedFiles.put(fileDep.key, fileDep);
                lock.unlock();

                client.get(fileDep.key);
            }
        }

        lock.lock();
        stillStartingUp = false;
        allDependenciesComplete.signalAll();
        lock.unlock();
    }

    /**
     * Handles returns of Get operation requested earlier.
     * @param event
     */
    private void operationReturned(OperationFinishedEvent event) {
        if(event.getCommandWord() != CommandWord.GET){
            return;
        }

        lock.lock();
        if(event.getOperation().isSuccess()){
            String key = event.getOperation().getKey();

            if(!unresolvedFiles.containsKey(key)){
                //TODO redirect output?
                System.out.println("FileDep with key ("+key+") wasn't found in Map for task with id "+taskMeta.taskName);
                //Might be from other request unrelated with this FileMaster
                return;
            }

            FileDep fileDep = unresolvedFiles.remove(key);

            Data result = (Data) event.getOperation().getResult();
            toFile(pathTo(fileDep), result.getData());

        } else {
            operationFailed = true;
            taskListener.taskFailed(taskName, "Failed to resolve file stored under key "+event.getOperation().getKey());
            allDependenciesComplete.signalAll();
        }

        if(unresolvedFiles.size()==0){
            allDependenciesComplete.signalAll();
        }
        lock.unlock();
    }

    private String getMetaFileName(String taskName){
        //TODO Do less hardcoded?
        return taskName+".json";
    }

    /**
     * Serialized data-class to Json
     *
     * Represents contents in one MetaTask file
     */
    private static class TaskMeta implements Serializable{
        private String projectName;
        private String taskName;

        private FileDep module;
        private List<FileDep> dependencies;

        private TaskMeta(String projectName, String taskName, FileDep module, List<FileDep> dependencies) {
            this.projectName = projectName;
            this.taskName = taskName;
            this.module = module;
            this.dependencies = dependencies;
        }
    }

    /**
     * Serialized data-class to Json
     *
     * Represent one file that has to be resolved for Task to compile or run
     */
    private static class FileDep implements Serializable{
        private String fileName;
        private String location;
        private String key;

        private boolean sticky = false;
        private int checkSum;

        private FileDep(String fileName, String location, String key, boolean sticky, int checkSum) {
            this.fileName = fileName;
            this.location = location;
            this.key = key;
            this.sticky = sticky;
            this.checkSum = checkSum;
        }
    }

    /**
     * Generates a suitable json-String to put in a file, used for debugging
     * @param args
     */
    public static void main(String[] args){
        FileDep rawIndata = new FileDep("2_2000.raw", "resources", "Primes_2_2000", false, 25);
        List<FileDep> deps = new ArrayList<FileDep>();
        deps.add(rawIndata);

        FileDep algorithm = new FileDep("Prime.hs", "code", "Primes_algorithms", true, 500);

        TaskMeta taskMetaTest = new TaskMeta("Primes", "PrimeTask_01", algorithm, deps);
        System.out.println( new Gson().toJson(taskMetaTest));
    }
}
