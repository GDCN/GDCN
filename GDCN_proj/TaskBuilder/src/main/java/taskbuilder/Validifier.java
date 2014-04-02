package taskbuilder;

import org.apache.commons.io.IOUtils;
import taskbuilder.communicationToClient.ValidityListener;

import java.io.IOException;
import java.io.StringWriter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by joakim on 2014-04-01.
 *
 * Class for validating results of tasks
 */
public class Validifier {

    private static final String ACCEPT_WORD = "ok.*";

    private final ValidityListener listener;

    /**
     * Creates a validifier instance
     * @param listener listens for validity results
     */
    public Validifier(ValidityListener listener) {
        this.listener = listener;
    }

    /**
     * Runs a validity test of a result
     * @param program the testing program
     * @param result the result file
     */
    public void testResult(String program, String result) {
        String[] command = {program, result};
        Process proc = null;

        try {
            proc = new ProcessBuilder(command).start();

            if (proc.waitFor() == 0) {
                // Check output
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getInputStream(), writer, null);

                Pattern pattern = Pattern.compile(ACCEPT_WORD, Pattern.DOTALL);
                Matcher matcher = pattern.matcher(writer.toString().toLowerCase());
                if (matcher.matches()) {
                    // Result accepted
                    listener.validityOk();
                }
                else {
                    // Result is corrupt
                    listener.validityCorrupt();
                }
            } else {
                // Program error
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getErrorStream(), writer, null);
                listener.validityError(writer.toString());
            }
        }
        catch (IOException | InterruptedException e) {
            listener.validityError(e.toString());
        }
        finally {
            if (proc != null)
                proc.destroy();
        }
    }

    public static void main(String[] args) {
        ValidityListener vl = new ValidityListener() {
            @Override
            public void validityOk() {
                System.out.println("Validity Ok");
            }

            @Override
            public void validityCorrupt() {
                System.out.println("Validity corrupt");
            }

            @Override
            public void validityError(String reason) {
                System.out.println("Validity error: " + reason);
            }
        };

        Validifier v = new Validifier(vl);
        v.testResult("someProgram", "someResultFile");
    }
}