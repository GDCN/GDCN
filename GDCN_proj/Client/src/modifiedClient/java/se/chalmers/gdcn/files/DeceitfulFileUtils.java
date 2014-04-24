package se.chalmers.gdcn.files;

import java.util.Set;
import java.util.TreeSet;

/**
 * Created by HalfLeif on 2014-04-02.
 */
public class DeceitfulFileUtils {

    public static String deduceTask(TaskMeta meta){
        final Set<String> files = new TreeSet<>();
        files.add(meta.getModule().getDhtKey());

        for(FileDep file:meta.getDependencies()){
            files.add(file.getDhtKey());
        }

        String answer = "";
        for(String s:files){
            answer += s;
        }

        return answer;
    }
}
