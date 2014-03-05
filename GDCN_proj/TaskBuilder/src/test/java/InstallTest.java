import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import taskbuilder.fileManagement.Install;
import taskbuilder.fileManagement.PathManager;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * Created by HalfLeif on 2014-03-04.
 */
public class InstallTest {

    private PathManager pathManager = new PathManager("Primes");

    @BeforeMethod
    public void setup(){
        Install.install();
    }

    @Test
    public void testInstall(){
        Install.install();
        assert isInstalled();
    }

    /**
     * This test passes, but might not always want to delete all saved progress on rebuild...
     */
    @Test
    public void testUninstall(){
//        Install.uninstall();
//        assert !isInstalled();
    }

    @Test
    public void deleteTemps(){
        File tempDir = new File(pathManager.projectTempDir());
        if(!tempDir.exists()){
            return;
        }

        pathManager.deleteTemps();
        assert !tempDir.exists();
    }

    @Test
    public void deleteBins(){
        File binDir = new File(pathManager.taskBinaryDir());
        if(!binDir.exists()){
            return;
        }

        pathManager.deleteBinaries();
        assert !binDir.exists();
    }

    /**
     * Checks if the path data exist.
     * @return
     */
    private static boolean isInstalled(){
        File rootPath = new File(Install.APPDATA);

        if(!rootPath.exists()){
            return false;
        }
        assert rootPath.isDirectory();

        File pathDataFile = new File(Install.PATH_DATA);
        if(!pathDataFile.exists()){
            return false;
        }
        assert !pathDataFile.isDirectory();

        Properties properties = new Properties();
        try {
            properties.load(new FileInputStream(pathDataFile));

        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }

        List<String> paths = new ArrayList<String>();

        paths.add(properties.getProperty("data_path"));
        paths.add(properties.getProperty("bin_path"));

        for(String str : paths){
            if(str==null){
                return false;
            }
        }

        return true;
    }

}
