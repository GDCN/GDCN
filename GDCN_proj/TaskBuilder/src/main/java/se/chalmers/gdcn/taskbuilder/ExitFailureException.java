package se.chalmers.gdcn.taskbuilder;

/**
 * Created by joakim on 2/26/14.
 */
public class ExitFailureException extends Exception {

    public ExitFailureException(String message) {
        super(message);
    }

    public ExitFailureException(String message, Throwable throwable) {
        super(message, throwable);
    }
}
