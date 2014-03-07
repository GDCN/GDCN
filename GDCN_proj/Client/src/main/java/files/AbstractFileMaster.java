package files;

import com.google.gson.Gson;
import command.communicationToUI.ClientInterface;
import command.communicationToUI.CommandWord;
import command.communicationToUI.OperationFinishedEvent;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.PathManager;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.*;
import java.util.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by HalfLeif on 2014-03-04.
 *
 * Abstract class for resolving task file dependencies.
 *
 * Uses TaskListener to report error information.
 */
abstract class AbstractFileMaster{

    protected final TaskMeta taskMeta;
    protected final PathManager pathManager;
    protected final ClientInterface client;

    private final TaskListener taskListener;
    private final CommandWord expectedOperation;

    protected final String taskName;

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
     * Creates FileMaster object that reads meta-file for a task. Run {@link FileMaster#runAndAwait()} for
     * solving the dependencies.
     *
     *
     * @param projectName Name of project
     * @param taskName Name of task
     * @param client Client for downloading files from network (DHT)
     * @param taskListener Listener to learn about failures such as unresolved dependencies.
     * @param expectedOperation
     * @throws FileNotFoundException if meta-file is not found. Path to search on is derived from projectName and taskName.
     */
    public AbstractFileMaster(String projectName, String taskName, ClientInterface client, TaskListener taskListener, CommandWord expectedOperation) throws FileNotFoundException, TaskMetaDataException {
        this.client = client;
        this.taskListener = taskListener;
        this.expectedOperation = expectedOperation;
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
     * Just runs {@link FileMaster#run()} and {@link FileMaster#await()}
     * @return result of {@link FileMaster#await()}
     */
    public boolean runAndAwait() throws TaskMetaDataException {
        run();
        return await();
    }

    /**
     * Attempts to resolve the dependencies found in meta-file.
     */
    private void run() throws TaskMetaDataException {
        if(taskMeta != null){
            resolveDependencies();
        } else {
            throw new TaskMetaDataException("Meta data file wasn't found (or parsed correctly)!");
        }
    }

    /**
     * Blocks current thread until all dependencies for the specified task are resolved.
     *
     * @return true if file has been properly downloaded, false if one of the dependencies couldn't be resolved.
     */
    private boolean await(){
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
     *
     * @return List of paths to all resource files mentioned in taskmetas
     */
    protected List<String> getResourceFiles() {
        List<String> resources = new ArrayList<String>();

        for(FileDep fileDep : taskMeta.dependencies){
            resources.add(pathTo(fileDep).getAbsolutePath());
        }

        return resources;
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

    protected File pathTo(FileDep fileDep){
        return new File(pathManager.projectDir() + fileDep.location + File.separator + fileDep.fileName);
    }

    /**
     * This file dependency was found locally. What to do?
     *
     * Called in resolve dependencies
     *
     * @param fileDep
     */
    protected abstract void ifFileExist(FileDep fileDep);

    /**
     * This file dependency wasn't found locally. What to do?
     *
     * Called in resolve dependencies
     *
     * @param fileDep
     */
    protected abstract void ifFileDoNotExist(FileDep fileDep);

    /**
     * Put file in waiting for dependency to be solved
     * @param fileDep
     */
    protected final void putDependentFile(FileDep fileDep){
        lock.lock();
        unresolvedFiles.put(fileDep.key, fileDep);
        lock.unlock();
    }

    protected final void removeDependentFile(FileDep fileDep){
        lock.lock();
        //TODO
        //when is this used?
    }

    /**
     * Attempt to solve dependecies that was found
     * @throws TaskMetaDataException if dependent File exist locally but is a directory
     */
    private void resolveDependencies() throws TaskMetaDataException {
        unresolvedFiles.clear();

        Set<FileDep> deps = new HashSet<FileDep>(taskMeta.dependencies);
        deps.add(taskMeta.module);

        for(FileDep fileDep : deps){
            File file = pathTo(fileDep);
            if(file.exists()){
                if(file.isDirectory()){
                    throw new TaskMetaDataException("Files in dependencies should not be directories! File: "
                            +file+" for task "+taskMeta.taskName);
                }
                ifFileExist(fileDep);
            } else {
                ifFileDoNotExist(fileDep);
            }
        }

        lock.lock();
        stillStartingUp = false;
        allDependenciesComplete.signalAll();
        lock.unlock();
    }

    /**
     * Operation successful with respect to this file
     * @param fileDep
     * @param result
     */
    protected abstract void operationForDependentFileCompleted(FileDep fileDep, Object result);

    /**
     * Handles returns of Get operation requested earlier.
     * @param event
     */
    private void operationReturned(OperationFinishedEvent event) {
        if(event.getCommandWord() != expectedOperation){
            return;
        }

        String key = event.getOperation().getKey();

        if(!unresolvedFiles.containsKey(key)){
            //TODO redirect output?
            System.out.println("FileDep with key ("+key+") wasn't found in Map for task with id "+taskMeta.taskName);
            //Might be from other request unrelated with this FileMaster
            return;
        }

        lock.lock();
        FileDep fileDep = unresolvedFiles.remove(key);

        if(event.getOperation().isSuccess()){

//            Data result = (Data) event.getOperation().getResult();
//            toFile(pathTo(fileDep), result.getData());
            operationForDependentFileCompleted(fileDep, event.getOperation().getResult());

        } else {
            operationFailed = true;
            taskListener.taskFailed(taskName, "Failed to resolve file with name " +  fileDep.fileName);
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
    protected static class TaskMeta implements Serializable{
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

        public String getProjectName() {
            return projectName;
        }

        public String getTaskName() {
            return taskName;
        }

        public FileDep getModule() {
            return module;
        }

        public List<FileDep> getDependencies() {
            return dependencies;
        }
    }

    /**
     * Serialized data-class to Json
     *
     * Represent one file that has to be resolved for Task to compile or run
     */
    protected static class FileDep implements Serializable{
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

        public String getFileName() {
            return fileName;
        }

        public String getLocation() {
            return location;
        }

        public String getKey() {
            return key;
        }

        public boolean isSticky() {
            return sticky;
        }

        public int getCheckSum() {
            return checkSum;
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
