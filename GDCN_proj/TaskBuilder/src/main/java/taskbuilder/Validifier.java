package taskbuilder;

import org.apache.commons.io.IOUtils;
import taskbuilder.communicationToClient.ValidityListener;

import java.io.IOException;
import java.io.StringWriter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by joakim on 2014-04-01.
 */
public class Validifier {

    private static final String ACCEPT_WORD = "ok.*";

    private final String program;
    private final ValidityListener listener;

    public Validifier(String program, ValidityListener listener) {
        this.program = program;
        this.listener = listener;
    }

    public void testResult(String worker, String taskName, String file) {
        String[] command = {program, file};
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
                    listener.validityOk(worker, taskName);
                }
                else {
                    // Result is corrupt
                    listener.validityCorrupt(worker, taskName);
                }
            } else {
                // Program error
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getErrorStream(), writer, null);
                listener.validityError(worker, taskName, writer.toString());
            }
        }
        catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
        finally {
            if (proc != null)
                proc.destroy();
        }
    }
}
