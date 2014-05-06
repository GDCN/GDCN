package se.chalmers.gdcn.files;

import net.tomp2p.peers.Number160;

import java.util.Set;
import java.util.TreeSet;

/**
 * Created by HalfLeif on 2014-04-02.
 */
public class DeceitfulFileUtils {

    public static String deduceTask(TaskMeta meta){
        final Set<Number160> files = new TreeSet<>();
        files.add(meta.getModule().getDhtKey());

        for(FileDep file:meta.getDependencies()){
            files.add(file.getDhtKey());
        }

        String answer = "";
        for(Number160 s:files){
            answer += s.toString();
        }

        return answer;
    }
}
