package taskbuilder.fileManagement;

import java.io.*;
import java.util.Properties;

/**
 * Singleton class for storing and loading paths
 */
public class OLD_PathManager {

    private static final String JOB_PATH = "Hjobcode/";
    private static final String INIT_DATA = "initdata/";
    private static final String EXECUTABLE = "exec/bin/";
    private static final String DUMP = "dump/";

    private static OLD_PathManager instance = null;

    private String resources = "ResourcesUndefined/";
    private String temp = "delete_me/";
    private String header = "HeaderUndefined";

    private OLD_PathManager() {
        loadDefaultLocation();
    }

    public static OLD_PathManager getInstance() {
        if (instance == null) {
            instance = new OLD_PathManager();
        }
        return instance;
    }

    private static void loadDefaultLocation(){
        try {
            instance.loadFromFile(Install.PATH_DATA);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     *
     * @param file Path of properties file containing the root paths for resources etc
     */
    public void loadFromFile(String file) {
        InputStream input = null;
        Properties prop = new Properties();
        try {
            input = new FileInputStream(file);
            prop.load(input);

            resources = prop.getProperty("resources");
            temp = prop.getProperty("temp");
            header = prop.getProperty("header");
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (input != null) {
                try {
                    input.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     *
     * @return Path to haskell source file to be executed
     */
    public String getJobCodePath() {
        return resources+JOB_PATH;
    }

    /**
     *
     * @return Path to directory containing compiled tasks
     */
    public String getJobExecutablePath() {
        return temp+EXECUTABLE;
    }

    /**
     *
     * @return Path to directory containing files with input data for a task.
     */
    public String getTaskInitDataPath() {
        return resources+INIT_DATA;
    }

    /**
     *
     * @return Path to haskell Header file that is running the task in Haskell
     */
    public String getHeaderPath() {
        return header;
    }

    /**
     *
     * @return Directory for temporary files such as object-files in compilation
     */
    public String getDumpPath() {
        return temp+DUMP;
    }
}
