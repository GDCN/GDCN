package se.chalmers.gdcn.communicationToUI;

/**
 * Created by HalfLeif on 2014-02-26.
 */
public interface WordInterface {

    /**
     * @return Number of arguments this command can take
     */
    int getArity();

    /**
     * @return Name of this command
     */
    String getName();

    /**
     * @return Arguments of this command
     */

    String getArguments();

    /**
     * @return Help string for this command
     */
    String getHelp();
}
