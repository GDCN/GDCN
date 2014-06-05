package se.chalmers.gdcn.taskbuilder.communicationToClient;

/**
 * Created by HalfLeif on 2014-04-02.
 */
public interface TaskFailureListener {

    void taskFailed(String taskName, String reason);
}
