package taskbuilder;

import java.io.IOException;

/**
 * Class for compiling and executing Haskell code
 */
public class HaskellRunner {

    //TODO Handle paths in another class
    private static final String GHC_PATH = "...";
    private static final String JOB_CODE_PATH = "...";
    private static final String JOB_EXECUTABLE_PATH = "...";
    private static final String HEADER_PATH = "...";

    // Compiles job code
    public static void compile(String module) throws IOException {
        String[] command = {GHC_PATH, "-o", JOB_EXECUTABLE_PATH + module, "-DMODULE=" + module, "-i" + JOB_CODE_PATH,
                            HEADER_PATH, "-trust", "base", "-trust", "bytestring", "-trust", "binary"};
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
