package se.chalmers.gdcn.taskbuilder;

import org.apache.commons.io.IOUtils;
import se.chalmers.gdcn.taskbuilder.fileManagement.Install;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.util.List;
import java.util.Map;

/**
 * Created by joakim on 2014-05-05.
 */
public class HaskellCompiler {

    public void compile(String[] command) throws IOException, InterruptedException, ExitFailureException{

        //System.out.println("\nCompile command:");
        //for(String c : command){
        //    System.out.print(c + " ");
        //}
        //System.out.println("\n");

        //ProcessBuilder pb = new ProcessBuilder(command).inheritIO();
        ProcessBuilder pb = new ProcessBuilder(command);

        Map<String, String> env = pb.environment();
        if (env.containsKey("GHC_PACKAGE_PATH")) {
            env.put("GHC_PACKAGE_PATH", Install.HDB_DIR + File.pathSeparator
                    + env.get("GHC_PACKAGE_PATH"));
        }
        else {
            env.put("GHC_PACKAGE_PATH", Install.HDB_DIR + File.pathSeparator);
        }

        Process proc = null;
        try {
            proc = pb.start();

            if (proc.waitFor() != 0) {
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getErrorStream(), writer, null);

                throw new ExitFailureException(writer.toString());
            }
        } finally {
            if (proc != null) {
                proc.destroy();
            }
        }
    }

    public void compile(List<String> command) throws IOException, InterruptedException, ExitFailureException{
        String[] array = new String[command.size()];
        compile(command.toArray(array));
    }
}
