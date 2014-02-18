import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * Created by Joakim on 2014-02-15.
 */
public class ERunner {

    public static final String E_CLASS_PATH = ".../GDCN_proj/Compute/src/main/resources";

    public static final String RUNE_PATH = ".../rune";

    public static final String ELOADER_PATH = ".../GDCN_proj/Compute/src/main/e/eloader.e";

    // Runs the E-maker file given as a module and returns the results as a string
    public static String runAlgorithm(String module) throws IOException {

        String[] commands = {RUNE_PATH, "-cpa", E_CLASS_PATH, ELOADER_PATH, module};
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
