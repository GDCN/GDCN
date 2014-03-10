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

    protected final List<String> taskNames = new ArrayList<>();

    protected final Map<String, TaskMeta> taskMetas = new HashMap<>();
    protected final PathManager pathManager;
    protected final ClientInterface client;

    private final TaskListener taskListener;
    private final CommandWord expectedOperation;

    private final Lock lock = new ReentrantLock();
    private final Condition allDependenciesComplete = lock.newCondition();
    private final Map<String, FileDep> unresolvedFiles = new HashMap<String, FileDep>();

    private volatile boolean operationFailed = false;
    private volatile boolean stillStartingUp = true;

    /**
     * Creates FileMaster object that reads meta-file for a task. Run {@link FileMaster#runAndAwait()} for
     * solving the dependencies.
     *
     *
     * @param projectName Name of project
     * @param taskNames Name of tasks
     * @param client Client for downloading files from network (DHT)
     * @param taskListener Listener to learn about failures such as unresolved dependencies.
     * @param expectedOperation
     * @throws FileNotFoundException if meta-file is not found. Path to search on is derived from projectName and taskName.
     */
    public AbstractFileMaster(String projectName, List<String> taskNames, ClientInterface client, TaskListener taskListener, CommandWord expectedOperation) throws FileNotFoundException, TaskMetaDataException {
        this.client = client;
        this.taskListener = taskListener;
        this.expectedOperation = expectedOperation;

        client.addListener(new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                if (evt instanceof OperationFinishedEvent) {
                    operationReturned((OperationFinishedEvent) evt);
                }
            }
        });

        pathManager = PathManager.worker(projectName);
        this.taskNames.addAll(taskNames);

        for(String taskName : taskNames){
            File metaTaskFile = new File(pathManager.taskMetaDir()+getMetaFileName(taskName));

            TaskMeta taskMeta = readMetaFile(metaTaskFile);
            taskMetas.put(taskName, taskMeta);

            if(! taskName.equals(taskMeta.taskName)){
                throw new TaskMetaDataException("Must be error in metaFile: taskName doesn't conform with filename!");
            }

            for(FileDep fileDep : taskMeta.dependencies){
                unresolvedFiles.put(fileDep.key, fileDep);
            }
            unresolvedFiles.put(taskMeta.module.key, taskMeta.module);
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
        if(taskMetas != null){
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

            System.out.println("Operation failed before enter loop, return FALSE");
            return false;
        }

        while(stillStartingUp || unresolvedFiles.size()>0){
            try {
                lock.lock();
                allDependenciesComplete.await();
                lock.unlock();
            } catch (InterruptedException e) {
                System.out.println("Caught interruption: "+e.getMessage());
                continue;
            }
            if(operationFailed){
                return false;
            }
            System.out.println("Test monitor condition before exit loop...");
        }

        //Alternatively, ignore await() model and use TaskListener instead...
        return true;
    }

    /**
     * Attempt to solve dependecies that was found
     * @throws TaskMetaDataException if dependent File exist locally but is a directory
     */
    private void resolveDependencies() throws TaskMetaDataException {
        //get ConcurrentModificationException if reads directly from unresolvedFiles.
        Set<FileDep> deps = new HashSet<FileDep>(unresolvedFiles.values());

        for(FileDep fileDep : deps){
            File file = pathTo(fileDep);
            if(file.exists()){
                if(file.isDirectory()){
                    throw new TaskMetaDataException("Files in dependencies should not be directories! File: "+file);
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
     * This file dependency was found locally. What to do?
     *
     * Called in resolve dependencies
     *
     * @param fileDep file
     */
    protected abstract void ifFileExist(FileDep fileDep);

    /**
     * This file dependency wasn't found locally. What to do?
     *
     * Called in resolve dependencies
     *
     * @param fileDep file
     */
    protected abstract void ifFileDoNotExist(FileDep fileDep);

    /**
     * Call to set a file dependency as resolved.
     * @param fileDep file
     */
    protected final void fileDependencyResolved(FileDep fileDep){
        lock.lock();
        unresolvedFiles.remove(fileDep.key);
        allDependenciesComplete.signalAll();
        lock.unlock();
    }

    /**
     * Operation successful with respect to this file
     * @param fileDep file
     * @param result Result of OperationEvent, may be null
     */
    protected abstract void operationForDependentFileSuccess(FileDep fileDep, Object result);

    /**
     * Handles returns of Get operation requested earlier.
     * @param event OperationFinishedEvent
     */
    private void operationReturned(OperationFinishedEvent event) {
        if(event.getCommandWord() != expectedOperation){
            return;
        }

        String key = event.getOperation().getKey();

        if(!unresolvedFiles.containsKey(key)){
            //TODO redirect output?
            System.out.println("FileDep with key ("+key+") wasn't found in Map for task");
            //Might be from other request unrelated with this FileMaster
            return;
        }

        lock.lock();
        FileDep fileDep = unresolvedFiles.remove(key);

        if(event.getOperation().isSuccess()){
            operationForDependentFileSuccess(fileDep, event.getOperation().getResult());

        } else {
            operationFailed = true;
            //TODO cannot just return any taskname...
            taskListener.taskFailed(taskNames.remove(0), "Failed to resolve file with name " +  fileDep.fileName);
            allDependenciesComplete.signalAll();
        }

        if(unresolvedFiles.size()==0){
            allDependenciesComplete.signalAll();
        }
        lock.unlock();
    }


    /**
     *
     * @param fileDep file
     * @return Absolute path to file
     */
    protected File pathTo(FileDep fileDep){
        return new File(pathManager.projectDir() + fileDep.location + File.separator + fileDep.fileName);
    }

    /**
     *
     * @return List of paths to all resource files mentioned in taskmetas
     */
    protected List<String> getResourceFiles(String taskName) {
        List<String> resources = new ArrayList<String>();


        for(FileDep fileDep : taskMetas.get(taskName).dependencies){
            resources.add(pathTo(fileDep).getAbsolutePath());
        }

        return resources;
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