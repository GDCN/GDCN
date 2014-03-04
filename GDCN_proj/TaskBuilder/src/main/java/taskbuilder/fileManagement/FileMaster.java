package taskbuilder.fileManagement;

import com.google.gson.Gson;

import java.io.*;
import java.util.List;

/**
 * Created by HalfLeif on 2014-03-04.
 */
public class FileMaster {

    private final TaskMeta taskMeta;
    private final PathManager pathManager;



    //TODO run asynchronously
    public FileMaster(String projectName, String taskName) {

        pathManager = new PathManager(projectName);
        File metaTaskFile = new File(pathManager.taskMetaDir()+getMetaFileName(taskName));

        taskMeta = readMetaFile(metaTaskFile);
        resolveDependencies();
    }

    private TaskMeta readMetaFile(File file){

        Reader reader = null;
        try {
            reader = new InputStreamReader(new BufferedInputStream(new FileInputStream(file)));

            Gson gson = new Gson();
            return gson.fromJson(reader, TaskMeta.class);

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return null;
    }

    private void resolveDependencies(){
        for(FileDep fileDep : taskMeta.dependencies){
            File file = new File(pathManager.projectDir() + fileDep.location + fileDep.fileName);
            if(file.exists()){
                if(file.isDirectory()){
                    throw new AssertionError("Files in dependencies should not be directories!");
                }
                //TODO checksum?
            } else {
                //TODO queue download from DHT
            }
        }
    }

    private String getMetaFileName(String taskName){
        return taskName+".prop";
    }

    private static class TaskMeta implements Serializable{
        private String projectName;
        private String taskId;

        private List<FileDep> dependencies;
    }

    private static class FileDep implements Serializable{
        private String fileName;
        private String location;
        private String key;

        private boolean sticky = false;
        private int checkSum;
    }


}
