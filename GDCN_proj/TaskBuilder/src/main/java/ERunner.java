import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * Created by Joakim on 2014-02-15.
 */
public class ERunner {

    public static final String JAVA_PATH = "...";

    public static final String JOBCODE_PATH = ".../GDCN_proj/Compute/src/main/resources";

    public static final String EJAR_PATH = ".../e.jar";

    public static final String ELOADER_PATH = ".../GDCN_proj/Compute/src/main/e/eloader.e";

    // Runs the E-maker file given as a module and returns the results as a string
    public static String runAlgorithm(String module) throws IOException {

        //TODO Possibly move to seperate class
        String separator;
        if (System.getProperty("os.name").startsWith("Windows")) {
            separator = ";";
        } else {
            separator = ":";
        }

        String eClassPath = EJAR_PATH + separator + JOBCODE_PATH;
        String[] commands = {JAVA_PATH, "-cp", eClassPath,
                    "org.erights.e.elang.interp.Rune", ELOADER_PATH, module};
        StringBuilder output = new StringBuilder();

        ProcessBuilder processBuilder = new ProcessBuilder(commands);

        Process proc = processBuilder.start();

        BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()));

        //TODO Detect and handle uncatchable errors (need to simulate enter press)
        String s = null;
        while ((s = stdInput.readLine()) != null) {
            output.append(s);
        }

        return output.toString();
    }

    // Quick test method
    /*public static void main(String[] args) {
        try {
            String res = runAlgorithm("jobcode.prime");
            System.out.println(res);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }*/
}
