package taskbuilder;

import org.apache.commons.io.IOUtils;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Class for tasks, compiling and executing Haskell code
 */
public class Task implements Runnable{

    private final String moduleName;
    private final String initData;

    private final TaskListener listener;

    public Task(String moduleName, String initData, TaskListener listener) {
        this.moduleName = moduleName;
        this.initData = initData;
        this.listener = listener;
    }

    /**
     * Compiles task code
     *
     * @throws IOException
     * @throws InterruptedException
     * @throws ExitFailureException
     */
    public void compile() throws IOException, InterruptedException, ExitFailureException {

        PathManager pathman = PathManager.getInstance();

        List<File> dirs = new ArrayList<File>();
        dirs.add(new File(pathman.getJobExecutablePath()));
        dirs.add(new File(pathman.getDumpPath()));

        for(File dir : dirs){
            if(!dir.exists()){
                dir.mkdirs();
            }
        }



        //TODO Manage trust in a non hardcoded way
        String[] command = {"ghc", "-o", pathman.getJobExecutablePath() + moduleName,
		        "-DMODULE=" + moduleName, "-i" + pathman.getJobCodePath(), pathman.getHeaderPath(),
                "-outputdir", pathman.getDumpPath(),
		        "-trust", "base", "-trust", "bytestring", "-trust", "binary"};
        Process proc = new ProcessBuilder(command).start();

        try {
            if (proc.waitFor() != 0) {
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getErrorStream(), writer, null);
                throw new ExitFailureException(writer.toString());
            }
        }
        catch (InterruptedException e) {
            proc.destroy();
            throw e;
        }
    }

    /**
     * Executes a task
     *
     * @return
     * @throws IOException
     * @throws InterruptedException
     * @throws ExitFailureException
     */
    public byte[] execute() throws IOException, InterruptedException, ExitFailureException {
        PathManager pathman = PathManager.getInstance();
        String[] command = {pathman.getJobExecutablePath() + moduleName,
                pathman.getTaskInitDataPath() + initData};

        Process proc = new ProcessBuilder(command).start();

        try {
            if (proc.waitFor() != 0) {
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getErrorStream(), writer, null);
                throw new ExitFailureException(writer.toString());
            }
            else {
                //TODO Possibly wrap pure result data in a class

//                outputStdErr(IOUtils.toString(proc.getErrorStream()));
//                outputStdErr(fromInstream(proc.getErrorStream()));
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getErrorStream(), writer, null);
                outputStdErr(writer.toString());
                return IOUtils.toByteArray(proc.getInputStream());
            }
        }
        catch (InterruptedException e) {
            proc.destroy();
            throw e;
        }
    }

    /**
     * Compiles and executes a task
     *
     * @return
     */
    @Override
    public void run(){
        String path = PathManager.getInstance().getJobExecutablePath() + moduleName;
        File executable = new File(path);
        try {
            if (executable.isDirectory()) {
                throw new IOException(path + " is a directory.");
            }
            if (!executable.exists()) {
                compile();
            }
            execute();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (ExitFailureException e) {
            e.printStackTrace();
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

    public static void toFile(byte[] results){
        String path = PathManager.getInstance().getDumpPath();
        BufferedOutputStream outputStream = null;
        try {
            outputStream = new BufferedOutputStream(new FileOutputStream(path));
            outputStream.write(results);
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

    public static void main(String[] args) throws IOException, InterruptedException, ExitFailureException {
        //NOTE: This test only works for Unix with current GDCN.properties
        // Directories /tmp/GDCN and /tmp/GDCNDump must also exist, they will be used
	    PathManager.getInstance().loadFromFile(System.getProperty("user.dir") +
                File.separator + "TaskBuilder/resources/pathdata.prop");
        Task t = new Task("Prime", "2_2000.raw", new TaskListener() {
            @Override
            public void taskFinished(String taskName) {
                //TODO
            }

            @Override
            public void taskFailed(String taskName, String reason) {
                //TODO
            }
        });
	    t.run();
    }
}
