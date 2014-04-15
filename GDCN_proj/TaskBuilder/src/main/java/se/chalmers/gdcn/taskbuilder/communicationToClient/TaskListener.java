package se.chalmers.gdcn.taskbuilder.communicationToClient;

/**
 * Created by HalfLeif on 2014-02-28.
 */
public interface TaskListener extends TaskFailureListener{

    void taskFinished(String taskName);


}
