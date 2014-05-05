package utils;

import com.google.gson.Gson;
import net.tomp2p.peers.Number160;
import se.chalmers.gdcn.files.TaskMeta;

/**
 * Created by Leif on 2014-04-17.
 */
public class TaskHolder {

    private final static Gson gson = new Gson();

    private final static String TASK_META_A = "{\n" +
            "    \"taskName\":\"PrimeTask_01\",\n" +
            "    \"module\":{\"fileName\":\"Prime.hs\",\"fileLocation\":\"code\",\"sticky\":true,\"checkSum\":500},\n" +
            "    \"dependencies\":\n" +
            "    [\n" +
            "        {\"fileName\":\"2_10000.raw\",\"fileLocation\":\"resources\",\"sticky\":false,\"checkSum\":25}\n" +
            "    ]\n" +
            "}";

    private final static String TASK_META_B = "{\n" +
            "    \"taskName\":\"PrimeTask_02\",\n" +
            "    \"module\":{\"fileName\":\"Prime.hs\",\"fileLocation\":\"code\",\"sticky\":true,\"checkSum\":500},\n" +
            "    \"dependencies\":\n" +
            "    [\n" +
            "        {\"fileName\":\"2_10000.raw\",\"fileLocation\":\"resources\",\"sticky\":false,\"checkSum\":25}\n" +
            "    ]\n" +
            "}";

    private static TaskMeta taskA = null;
    private static TaskMeta taskB = null;

    private static TaskMeta parse(String task){
        return gson.fromJson(task, TaskMeta.class);
    }

    public static TaskMeta getTaskA() {
        if(taskA == null){
            taskA = parse(TASK_META_A);
            taskA.getModule().setDhtKey(Number160.createHash(taskA.getModule().getFileName()));
        }
        return taskA;
    }

    public static TaskMeta getTaskB() {
        if(taskB == null){
            taskB = parse(TASK_META_B);
            taskB.getModule().setDhtKey(Number160.createHash(taskB.getModule().getFileName()));
        }
        return taskB;
    }
}
