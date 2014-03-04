package taskbuilder.fileManagement;

import java.io.*;
import java.util.Properties;

/**
 * Created by HalfLeif on 2014-03-04.
 *
 * Manages paths specific to a project
 */
public class PathManager {

    private final String projectName;

    private final static String RAW_FOLDER_NAME = "resources" + File.separator;
    private final static String TASK_FOLDER_NAME = "tasks" + File.separator;
    private final static String CODE_FOLDER_NAME = "code" + File.separator;
    private final static String BIN_FOLDER_NAME = "bin" + File.separator;
    private final static String DUMP_FOLDER_NAME = "temp" + File.separator;

    private static String headerLocation = null;
    private static String dataPath = null;

    public PathManager(String projectName) {
        this.projectName = projectName;

        loadDefaultLocation();
    }

    private void check(){
        if(dataPath == null || headerLocation == null){
            throw new AssertionError("Paths has not been read properly!");
        }
    }

    /**
     *
     * @return Project name
     */
    public String getProjectName() {
        return projectName;
    }

    /**
     *
     * @return Path to Header.hs
     */
    public String header(){
        check();
        return headerLocation + Install.HEADER_NAME;
    }

    /**
     *
     * @return Path to directory for input and output files of tasks, ie raw data files.
     */
    public String taskDataDir(){
        check();
        return dataPath + projectName + File.separator + RAW_FOLDER_NAME;
    }

    /**
     *
     * @return Path to directory with meta information about tasks.
     */
    public String taskMetaDir(){
        check();
        return dataPath + projectName + File.separator + TASK_FOLDER_NAME;
    }

    /**
     *
     * @return Path to directory with source code for task algorithm
     */
    public String taskCodeDir(){
        check();
        return dataPath + projectName + File.separator + CODE_FOLDER_NAME;
    }

    /**
     *
     * @return Path to directory with compiled source code for task algorithm
     */
    public String taskBinaryDir(){
        check();
        return dataPath + projectName + File.separator + BIN_FOLDER_NAME;
    }

    /**
     *
     * @return Path to directory for temp files
     */
    public String taskDumpDir(){
        check();
        return dataPath + projectName + File.separator + DUMP_FOLDER_NAME;
    }

    /**
     * Load property file in default location as specified by {@link Install#PATH_DATA}
     */
    public static void loadDefaultLocation(){
        try {
            loadFromFile(Install.PATH_DATA);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     *
     * @param file Path of properties file containing the root paths for resources etc
     */
    public static void loadFromFile(String file) {
        InputStream input = null;
        Properties prop = new Properties();
        try {
            input = new FileInputStream(file);
            prop.load(input);

            headerLocation = prop.getProperty("bin_path");
            dataPath = prop.getProperty("data_path");
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


}
