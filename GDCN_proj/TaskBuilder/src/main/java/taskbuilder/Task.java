package taskbuilder;

import org.apache.commons.io.IOUtils;

import java.io.IOException;
import java.io.StringWriter;

/**
 * Class for tasks, compiling and executing Haskell code
 */
public class Task {

    private final String moduleName;

    public Task(String module) {
        moduleName = module;
    }

    // Compiles job code
    public void compile() throws IOException, InterruptedException, ExitFailureException {
	    PathManager pathman = PathManager.getInstance();
        //TODO Manage trust in a non hardcoded way
        String[] command = {pathman.getGhcPath(), "-o", pathman.getJobExecutablePath() + moduleName,
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

    // Executes a task
    public byte[] execute(String[] inputFiles) throws IOException, InterruptedException, ExitFailureException {
        PathManager pathman = PathManager.getInstance();
        String[] command = new String[inputFiles.length + 1];

        command[0] = pathman.getJobExecutablePath() + moduleName;
        for (int i = 0; i < inputFiles.length; i++) {
            command[i+1] = inputFiles[i];
        }

        Process proc = new ProcessBuilder(command).start();

        try {
            if (proc.waitFor() != 0) {
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getErrorStream(), writer, null);
                throw new ExitFailureException(writer.toString());
            }
            else {
                //TODO Possibly wrap pure result data in a class
                return IOUtils.toByteArray(proc.getInputStream());
            }
        }
        catch (InterruptedException e) {
            proc.destroy();
            throw e;
        }
    }

    public static void main(String[] args) throws IOException, InterruptedException, ExitFailureException {
	    PathManager.getInstance().loadFromFile("GDCN.properties");
        Task t = new Task("Prime");
	    t.compile();
        String[] in = {};
	    byte[] res = t.execute(in);
        System.out.println(res);
    }
}
