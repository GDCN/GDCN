package se.chalmers.gdcn.taskbuilder;

import org.apache.commons.io.IOUtils;
import se.chalmers.gdcn.taskbuilder.communicationToClient.ValidityListener;

import java.io.IOException;
import java.io.StringWriter;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * Created by joakim on 2014-04-01.
 *
 * Class for validating results of tasks
 */
public class Validifier {

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
     * @param taskDeps the task dependencies used for generating the result
     */
    public void testResult(String program, String result, List<String> taskDeps) {
        List<String> command = new ArrayList<>();
        command.add(program);
        command.add(result);
        command.addAll(taskDeps);
        Process proc = null;

        try {
            proc = new ProcessBuilder(command).start();

            if (proc.waitFor() == 0) {
                // Check output
                StringWriter writer = new StringWriter();
                IOUtils.copy(proc.getInputStream(), writer, null);

                try {
                    NumberFormat numberFormat = NumberFormat.getInstance(Locale.ENGLISH);
                    double quality = numberFormat.parse(writer.toString()).doubleValue();

                    // Result accepted
                    listener.validityOk(quality);
                }
                catch (ParseException e) {
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
}