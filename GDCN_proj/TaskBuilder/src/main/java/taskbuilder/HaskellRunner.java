package taskbuilder;

import org.apache.commons.io.IOUtils;

import java.io.IOException;

/**
 * Class for compiling and executing Haskell code
 */
public class HaskellRunner {

    // Compiles job code
    public static void compile(String module) throws IOException, InterruptedException {
	    PathManager pathman = PathManager.getInstance();
        String[] command = {pathman.getGhcPath(), "-o", pathman.getJobExecutablePath() + module,
			    "-DMODULE=" + module, "-i" + pathman.getJobCodePath(), pathman.getHeaderPath(),
                "-outputdir", pathman.getDumpPath(),
			    "-trust", "base", "-trust", "bytestring", "-trust", "binary"};
        Process proc = new ProcessBuilder(command).start();

        if (proc.waitFor() != 0) {
            //Lazy error reporting
            IOUtils.copy(proc.getErrorStream(), System.err);
            //TODO Throw better exception (or return a false like value)
            throw new RuntimeException();
        }
    }

    // Executes a task
    public static byte[] execute(String module, String[] inputFiles) throws IOException, InterruptedException{
        PathManager pathman = PathManager.getInstance();
        String[] command = new String[inputFiles.length + 1];

        command[0] = pathman.getJobExecutablePath() + module;
        for (int i = 0; i < inputFiles.length; i++) {
            command[i+1] = inputFiles[i];
        }

        Process proc = new ProcessBuilder(command).start();

        if (proc.waitFor() != 0) {
            //Lazy error reporting
            IOUtils.copy(proc.getErrorStream(), System.err);
            //TODO Throw better exception (or return a false like value)
            throw new RuntimeException();
        }
        else {
            //TODO Possibly wrap pure result data in a class
            return IOUtils.toByteArray(proc.getInputStream());
        }
    }

    public static void main(String[] args) throws IOException, InterruptedException {
	    PathManager.getInstance().loadFromFile("GDCN.properties");
	    compile("Prime");
        String[] in = {};
	    byte[] res = execute("Prime", in);
        System.out.println(res);
    }
}
