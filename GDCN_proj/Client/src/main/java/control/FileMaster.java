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
 * Can be run either asynchronously or synchronously.
 *
 * Uses TaskListener to report error information.
 * Use {@link FileMaster#await()} to see when finished.
 */
public class FileMaster implements Runnable{

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

    public FileMaster(String projectName, String taskName, ClientInterface client, TaskListener taskListener) {
        this.client = client;
        this.taskListener = taskListener;
        client.addListener(propertyListener);

        this.taskName = taskName;
        pathManager = new PathManager(projectName);
        File metaTaskFile = new File(pathManager.taskMetaDir()+getMetaFileName(taskName));

        taskMeta = readMetaFile(metaTaskFile);
    }

    @Override
    public void run(){
        if(taskMeta != null){
            resolveDependencies();
        }
    }

    public boolean runAndAwait(){
        run();
        return await();
    }

    /**
     * Blocks current thread until all dependencies for the specified task are resolved.
     *
     * @return true if file has been properly downloaded, false if one of the dependencies couldn't be resolved.
     */
    public boolean await(){
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
     * Outputs data to file
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


    private TaskMeta readMetaFile(File file) {

        Reader reader = null;
        try {
            reader = new InputStreamReader(new BufferedInputStream(new FileInputStream(file)));

            Gson gson = new Gson();
            return gson.fromJson(reader, TaskMeta.class);

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return null;
    }

    private File pathTo(FileDep fileDep){
        return new File(pathManager.projectDir() + fileDep.location + File.separator + fileDep.fileName);
    }

    private void resolveDependencies(){
        unresolvedFiles.clear();

        List<FileDep> deps = new ArrayList<FileDep>(taskMeta.dependencies);
        deps.add(taskMeta.module);

        for(FileDep fileDep : deps){
            File file = pathTo(fileDep);
            if(file.exists()){
                if(file.isDirectory()){
                    throw new AssertionError("Files in dependencies should not be directories!");
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

    private void operationReturned(OperationFinishedEvent event) {
        if(event.getCommandWord() != CommandWord.GET){
            return;
        }

        lock.lock();
        if(event.getOperation().isSuccess()){
            String key = event.getOperation().getKey();
            FileDep fileDep = unresolvedFiles.remove(key);

            if(fileDep == null) {
                throw new AssertionError("FileDep wasn't found in Map: "+fileDep.fileName);
            }

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

    private static class TaskMeta implements Serializable{
        private String projectName;
        private String taskId;

        private FileDep module;
        private List<FileDep> dependencies;

        private TaskMeta(String projectName, String taskId, FileDep module, List<FileDep> dependencies) {
            this.projectName = projectName;
            this.taskId = taskId;
            this.module = module;
            this.dependencies = dependencies;
        }
    }

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

    public static void main(String[] args){
        FileDep rawIndata = new FileDep("2_2000.raw", "resources", "Primes_2_2000", false, 25);
        List<FileDep> deps = new ArrayList<FileDep>();
        deps.add(rawIndata);

        FileDep algorithm = new FileDep("Prime.hs", "code", "Primes_algorithms", true, 500);

        TaskMeta taskMetaTest = new TaskMeta("Primes", "PrimeTask_01", algorithm, deps);
        System.out.println( new Gson().toJson(taskMetaTest));
    }
}
