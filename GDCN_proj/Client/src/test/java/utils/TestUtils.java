package utils;

import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.replica.ReplicaManager;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Created by HalfLeif on 2014-04-16.
 */
public class TestUtils {

    public static void nap(int millis){
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public static void print(Set<String> set){
        for(String s : set){
            System.out.println(s);
        }
    }

    public static void loadMeta(TaskMeta taskMeta, ReplicaManager replicaManager){
        List<TaskMeta> taskMetas = new ArrayList<>();
        taskMetas.add(taskMeta);
        replicaManager.loadTasksAndReplicate("Job1", taskMetas);
    }
}
