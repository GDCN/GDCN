package se.chalmers.gdcn.control;

import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;

/**
 * Created by Leif on 2014-04-23.
 */
public interface TaskRunner {
    void submit(Runnable runnable);

    TaskListener getTaskListener();
}
