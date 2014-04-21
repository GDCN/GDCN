package se.chalmers.gdcn.taskbuilder.fileManagement;

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
    private final static String TEMP_FOLDER_NAME = "temp" + File.separator;
    private final static String VALID_FOLDER_NAME = "valid" + File.separator;

    private static String headerLocation = null;
    private static String dataPath = null;
    private static String jobPath = null;
    private static String settingsPath = null;

    private final boolean isWorker;

    /**
     * Points to data folder
     * @param projectName Name of project to work on
     * @return PathManager instance
     */
    public static PathManager worker(String projectName){
        return new PathManager(projectName, true);
    }

    /**
     * Points to job folder
     * @param jobName Name of JobFolder
     * @return PathManager instance
     */
    public static PathManager jobOwner(String jobName){
        return new PathManager(jobName, false);
    }

    private PathManager(String workingDirectory, boolean isWorker) {
        this.projectName = workingDirectory;

        if(dataPath == null || headerLocation == null){
            loadDefaultLocation();
        }
        this.isWorker = isWorker;
    }

    private static void check(){
        if(dataPath == null || headerLocation == null || jobPath == null || settingsPath == null){
            throw new AssertionError("Paths has not been read properly!");
        }
    }

    /**
     *
     * @return Path to settings folder
     */
    public static String getSettingsPath() {
        check();
        return settingsPath;
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
     * @return Project name if isWorker, else JobFolderName
     */
    public String getProjectName() {
        return projectName;
    }

    public String getResultFilePath(String taskName){
        return taskResourcesDir() + taskName + ".result";
    }

    /**
     *
     * @return Project directory path
     */
    public String projectDir(){
        check();
        String location = isWorker? dataPath : jobPath;
        return location + projectName + File.separator;
    }

    /**
     *
     * @return Path to directory for input and output files of tasks, ie raw data files.
     */
    public String taskResourcesDir(){
        check();
        return projectDir() + RAW_FOLDER_NAME;
    }

    /**
     *
     * @return Path to directory with meta information about tasks.
     */
    public String taskMetaDir(){
        check();
        return projectDir() + TASK_FOLDER_NAME;
    }

    /**
     *
     * @return Path to directory with source code for task algorithm
     */
    public String taskCodeDir(){
        check();
        return projectDir() + CODE_FOLDER_NAME;
    }

    /**
     *
     * @return Path to directory with compiled source code for task algorithm
     */
    public String taskBinaryDir(){
        check();
        return projectDir() + BIN_FOLDER_NAME;
    }

    /**
     *
     * @return Path to directory for all temp files in project
     */
    public String projectTempDir(){
        check();
        return projectDir() + TEMP_FOLDER_NAME;
    }

    /**
     *
     * @return Path to directory for all temp files in task
     */
    public String taskTempDir(String taskName){
        check();
        return projectDir() + TEMP_FOLDER_NAME + taskName;
    }

    /**
     *
     * @return Path to directory for validation program of project
     */
    public String projectValidDir(){
        check();
        return projectDir() + VALID_FOLDER_NAME;
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
            jobPath = prop.getProperty("job_path");
            settingsPath = prop.getProperty("settings_path");
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
     * Delete all temp files for this project
     * @return if success
     */
    public boolean deleteTemps(){
        return Install.deleteContents(new File(this.projectTempDir()));
    }

    /**
     * Delete all temp files for this task
     * @return if success
     */
    public boolean deleteTaskTemp(String taskName){
        return Install.deleteContents(new File(this.taskTempDir(taskName)));
    }

    /**
     * Delete all binaries for this project
     * @return if success
     */
    public boolean deleteBinaries(){
        return Install.deleteContents(new File(this.taskBinaryDir()));
    }

}
