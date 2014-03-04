package taskbuilder.communicationToClient;

/**
 * Created by HalfLeif on 2014-02-28.
 */
public interface TaskListener {

    void taskFinished(String taskName);

    void taskFailed(String taskName, String reason);

}
