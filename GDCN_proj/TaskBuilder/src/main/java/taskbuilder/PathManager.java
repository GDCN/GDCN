package taskbuilder;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Singleton class for storing and loading paths
 */
public class PathManager {

    private static PathManager instance = null;

    private String jobCodePath;
    private String jobExecutablePath;
    private String taskInitDataPath;
    private String headerPath;
    private String dumpPath;

    private PathManager() {}

    public static PathManager getInstance() {
        if (instance == null) {
            instance = new PathManager();
        }
        return instance;
    }

    public void loadFromFile(String file) {
        InputStream input = null;
        Properties prop = new Properties();
        try {
            input = new FileInputStream(file);
            prop.load(input);

            jobCodePath = prop.getProperty("jobCodePath");
            jobExecutablePath = prop.getProperty("jobExecutablePath");
            taskInitDataPath = prop.getProperty("taskInitDataPath");
            headerPath = prop.getProperty("headerPath");
            dumpPath = prop.getProperty("dumpPath");
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

    public String getJobCodePath() {
        return jobCodePath;
    }

    public String getJobExecutablePath() {
        return jobExecutablePath;
    }

    public String getTaskInitDataPath() {
        return taskInitDataPath;
    }

    public String getHeaderPath() {
        return headerPath;
    }

    public String getDumpPath() {
        return dumpPath;
    }
}
