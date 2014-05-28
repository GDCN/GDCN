package se.chalmers.gdcn.taskbuilder;

import org.apache.commons.io.IOUtils;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;
import se.chalmers.gdcn.taskbuilder.utils.FormatString;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Class for tasks, compiling and executing Haskell code
 */
public class Task implements Runnable{

    private static final String[] trustedPackages = {"base", "bytestring", "mtl", "random", "gdcn-trusted"};

    private final String projectName;
    private final String taskName;
    private final String moduleName;
    private final List<String> initDataPaths;

    private final PathManager pathManager;

    private final TaskListener listener;

    /**
     *
     * @param projectName Name of local folder to work in
     * @param taskName Name of the task
     * @param moduleName Name of the Haskell module
     * @param initDataFiles List of paths to respective resource files
     * @param listener Listener on success or failure
     */
    public Task(String projectName, String taskName, String moduleName, List<String> initDataFiles, TaskListener listener) {
        this.projectName = projectName;
        this.taskName = taskName;
        this.moduleName = moduleName;
        this.initDataPaths = new ArrayList<>(initDataFiles);
        this.listener = listener;

        pathManager = PathManager.worker(this.projectName);
    }

    private String compiledModule(){
        return pathManager.taskBinaryDir() + moduleName;
    }

    /**
     * Compiles task code
     */
    public void compile(){
        List<File> dirs = new ArrayList<>();
        dirs.add(new File(pathManager.taskBinaryDir()));
        dirs.add(new File(pathManager.projectTempDir()));

        for(File dir : dirs){
            if(!dir.exists()){
                dir.mkdirs();
            }
        }

        //TODO Manage trust in a non hardcoded way
        String[] commandInit = {"ghc", "-o", compiledModule(),
                "-DMODULE=" + moduleName, "-i" + pathManager.taskCodeDir(), pathManager.header(),
                "-outputdir", pathManager.taskTempDir(taskName)};

        List<String> command = new ArrayList<>();
        command.addAll(Arrays.asList(commandInit));

        for (String trustedPackage : trustedPackages) {
            command.add("-trust");
            command.add(trustedPackage);
        }

        HaskellCompiler haskellCompiler = new HaskellCompiler();

        // For demo
        System.out.println("Compiling task...");
        System.out.println();

        try {
            haskellCompiler.compile(command);
        }
        catch (InterruptedException | IOException | ExitFailureException e) {
            e.printStackTrace();
            listener.taskFailed(moduleName, e.getMessage());
        } finally {
            pathManager.deleteTaskTemp(taskName);
        }
    }

    /**
     * Executes a task
     */
    public void execute(){
        List<String> command = new ArrayList<>();
        command.add(compiledModule());
        final String resultFile = pathManager.getResultFilePath(taskName);
        command.add(resultFile);
        command.addAll(initDataPaths);

        //TODO remove this output?
        //System.out.println("\nRun command:");
        // For demo
        //for(String c : command){
        //    System.out.print(c + " ");
        //}
        //System.out.println("\n");

        System.out.println("Running task...");
        System.out.println();

        Process proc = null;

        try {
            //proc = new ProcessBuilder(command).inheritIO().start();
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

                // Currently haskell doesn't print anything to stderr...
                //StringWriter writer = new StringWriter();
                //IOUtils.copy(proc.getErrorStream(), writer, null);
                //outputStdErr(writer.toString());

                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getInputStream(), writer, null);
                System.out.println("Result of task:");
                System.out.println(FormatString.colour(writer.toString(), FormatString.Colour.GREEN));
                System.out.println();

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
        String execFilePath = compiledModule();
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

    private static void outputStdErr(String output){
        //TODO output in more general fashion
        //Cleaned up for demo
        System.out.println();
        //System.out.println("\n-- StdErr:");
        //System.out.println(output);
        //System.out.println("-- end of StdErr.");
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
