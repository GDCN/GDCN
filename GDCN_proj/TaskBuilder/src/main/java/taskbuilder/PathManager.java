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

    private static final String JOB_PATH = "Hjobcode/";
    private static final String INIT_DATA = "initdata/";
    private static final String EXECUTABLE = "exec/bin/";
    private static final String DUMP = "dump/";

    private static PathManager instance = null;

    private String resources;
    private String temp;
    private String header;

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

    public String getJobCodePath() {
        return resources+JOB_PATH;
    }

    public String getJobExecutablePath() {
        return temp+EXECUTABLE;
    }

    public String getTaskInitDataPath() {
        return resources+INIT_DATA;
    }

    public String getHeaderPath() {
        return header;
    }

    public String getDumpPath() {
        return temp+DUMP;
    }
}
