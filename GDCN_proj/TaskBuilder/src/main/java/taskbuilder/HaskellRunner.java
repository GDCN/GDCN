package taskbuilder;

import java.io.IOException;

/**
 * Class for compiling and executing Haskell code
 */
public class HaskellRunner {

    // Compiles job code
    public static void compile(String module) throws IOException {
	PathManager pathman = PathManager.getInstance();
        String[] command = {pathman.getGhcPath(), "-o", pathman.getJobExecutablePath() + module,
			    "-DMODULE=" + module, "-i" + pathman.getJobCodePath(), pathman.getHeaderPath(),
			    "-trust", "base", "-trust", "bytestring", "-trust", "binary"};
        Process proc = new ProcessBuilder(command).start();

        if (proc.exitValue() != 0) {
            //TODO Throw better exception (or return a false like value)
            throw new RuntimeException();
        }
    }

    // Executes a task
    public static void execute(String module, String[] inputFiles) {
        //TODO
    }
}
