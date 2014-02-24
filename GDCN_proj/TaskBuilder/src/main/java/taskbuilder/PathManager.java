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

    private String ghcPath;
    private String jobCodePath;
    private String jobExecutablePath;
    private String headerPath;

    private PathManager() {}

    public static PathManager getInstance() {
        if (instance == null) {
            instance = new PathManager();
        }
        return instance;
    }

    public void loadFromFile(String file) {
        Properties prop = new Properties();
        try {
            InputStream input = new FileInputStream(file);
            prop.load(input);
            input.close();
            ghcPath = prop.getProperty("ghcPath");
            jobCodePath = prop.getProperty("jobCodePath");
            jobExecutablePath = prop.getProperty("jobExecutablePath");
            headerPath = prop.getProperty("headerPath");
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public String getGhcPath() {
        return ghcPath;
    }

    public String getJobCodePath() {
        return jobCodePath;
    }

    public String getJobExecutablePath() {
        return jobExecutablePath;
    }

    public String getHeaderPath() {
        return headerPath;
    }
}
