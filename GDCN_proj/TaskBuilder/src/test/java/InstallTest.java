import org.testng.annotations.Test;
import taskbuilder.fileManagement.Install;

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
