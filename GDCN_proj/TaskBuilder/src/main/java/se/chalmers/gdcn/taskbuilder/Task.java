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
        System.out.println();
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

                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getInputStream(), writer, null);
                System.out.println("Result of task:");
                System.out.println(FormatString.colour(writer.toString(), FormatString.Colour.GREEN));
                System.out.println();

                listener.taskFinished(taskName);
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
}
