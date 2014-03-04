package taskbuilder;

import org.apache.commons.io.IOUtils;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.PathManager;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Class for tasks, compiling and executing Haskell code
 */
public class Task implements Runnable{

    private final String projectName;
    private final String taskName;
    private final String moduleName;
    private final String initDataPaths;


    private final PathManager pathManager;


    private final TaskListener listener;

    public Task(String projectName, String taskName, String moduleName, List<String> initDataFiles, TaskListener listener) {
        this.projectName = projectName;
        this.taskName = taskName;
        this.moduleName = moduleName;
        this.initDataPaths = buildInitData(initDataFiles);
        this.listener = listener;

        pathManager = new PathManager(this.projectName);
    }

    private String buildInitData(List<String> initDataFiles){
        String result = "";
        for(String path : initDataFiles){
            result += path + " ";
        }
        return result;
    }

    /**
     * Compiles task code
     */
    public void compile(){
        List<File> dirs = new ArrayList<File>();
        dirs.add(new File(pathManager.taskBinaryDir()));
        dirs.add(new File(pathManager.taskTempDir()));

        for(File dir : dirs){
            if(!dir.exists()){
                dir.mkdirs();
            }
        }

        //TODO Manage trust in a non hardcoded way
        String[] command = {"ghc", "-o", pathManager.taskBinaryDir() + moduleName,
                "-DMODULE=" + moduleName, "-i" + pathManager.taskCodeDir(), pathManager.header(),
                "-outputdir", pathManager.taskTempDir(),
                "-trust", "base", "-trust", "bytestring", "-trust", "binary"};

        Process proc = null;
        try {
            proc = new ProcessBuilder(command).start();

            if (proc.waitFor() != 0) {
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getErrorStream(), writer, null);

                throw new ExitFailureException(writer.toString());
            }
        }
        catch (Exception e) {
            if (proc != null) {
                proc.destroy();
            }
            e.printStackTrace();
            listener.taskFailed(moduleName, e.getMessage());
        }
    }

    /**
     * Executes a task
     */
    public void execute(){
        String[] command = {pathManager.taskBinaryDir() + moduleName,
                pathManager.taskResourcesDir() + taskName + ".result",
                initDataPaths};


        for(String c : command){
            System.out.println(c);
        }
        //TODO fix bug, says cannot open binary file that actually exists...
        //If run these commands in bash, they work... :P

        Process proc = null;

        try {
            proc = new ProcessBuilder(command).start();

            if (proc.waitFor() != 0) {
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getErrorStream(), writer, null);
                throw new ExitFailureException(writer.toString());
            }
            else {
                //TODO Possibly wrap pure result data in a class

//                outputStdErr(IOUtils.toString(proc.getErrorStream()));
//                outputStdErr(fromInstream(proc.getErrorStream()));

                // TODO Currently haskell doesn't print any to stderr...
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getErrorStream(), writer, null);
                outputStdErr(writer.toString());

                listener.taskFinished(taskName);
//                return IOUtils.toByteArray(proc.getInputStream());
            }
        }
        catch (Exception e) {
            if (proc != null) {
                proc.destroy();
            }
            e.printStackTrace();
            listener.taskFailed(taskName, e.getMessage());
        }
    }

    /**
     * Compiles and executes a task
     */
    @Override
    public void run(){
        String execFilePath = pathManager.taskBinaryDir() + moduleName;
        File executable = new File(execFilePath);
        try {
            if (executable.isDirectory()) {
                throw new IOException(execFilePath + " is a directory.");
            }
            if (!executable.exists()) {
                compile();
            }
            execute();
        } catch (Exception e) {
            e.printStackTrace();
            listener.taskFailed(taskName, e.getMessage());
        }
    }

    public static String fromInstream(InputStream inputStream){
        StringBuilder stringBuilder = new StringBuilder();
        BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));

        String line;

        try {
            while( (line = reader.readLine()) != null){
                stringBuilder.append(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }


        return stringBuilder.toString();
    }

    public static void outputStdErr(String output){
        //TODO output in more general fashion
        System.out.println("-- StdErr:");
        System.out.println(output);
    }

//    public static void main(String[] args) throws IOException, InterruptedException, ExitFailureException {
//        //NOTE: This test only works for Unix with current GDCN.properties
//        // Directories /tmp/GDCN and /tmp/GDCNDump must also exist, they will be used
//	    OLD_PathManager.getInstance().loadFromFile(System.getProperty("user.dir") +
//                File.separator + "TaskBuilder/resources/pathdata.prop");
//        Task t = new Task("TaskName_Prime_1", "Prime", "2_2000.raw", new TaskListener() {
//            @Override
//            public void taskFinished(String taskName) {
//                //TODO
//            }
//
//            @Override
//            public void taskFailed(String taskName, String reason) {
//                //TODO
//            }
//        });
//	    t.run();
//    }
}
