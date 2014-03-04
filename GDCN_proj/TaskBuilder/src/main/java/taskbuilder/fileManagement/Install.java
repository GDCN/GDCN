package taskbuilder.fileManagement;

import java.io.*;
import java.util.Properties;

/**
 * Created by HalfLeif on 2014-03-04.
 */
public class Install {

    private static final String SEPARATOR = File.separator;

    public static final String APPDATA = System.getProperty("user.home") + SEPARATOR + ".gdcn" + SEPARATOR;
    public static final String PATH_DATA = APPDATA + "pathdata.prop";
    public static final String HEADER_NAME = "Header.hs";

    /**
     * Simply runs {@link Install#install()}
     * @param args
     */
    public static void main(String[] args){
        install();
    }

    /**
     * Creates directory for application data. Creates file containing important paths used by application.
     * Must be run from GDCN_proj/ directory.
     */
    public static void install(){
        File rootPath = new File(APPDATA);
        rootPath.mkdirs();

        File pathDataFile = new File(PATH_DATA);

        OutputStream outputStream = null;
        try {
            outputStream = new BufferedOutputStream(new FileOutputStream(pathDataFile));

            Properties pathData = paths();
            pathData.store(outputStream, " -- Paths for GDCN --");

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if(outputStream != null){
                try {
                    outputStream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Deletes entire directory for application data
     */
    public static boolean uninstall(){
        File rootPath = new File(APPDATA);
        return deleteContents(rootPath);
    }

    /**
     * Deletes entire directory with contents. Use with care!
     * http://stackoverflow.com/questions/7768071/how-to-delete-folder-content-in-java
     * @param directory
     * @return
     */
    private static boolean deleteContents(File directory){
        File[] files = directory.listFiles();
        if(files!=null) { //some JVMs return null for empty dirs
            for(File f: files) {
                if(f.isDirectory()) {
                    deleteContents(f);
                } else {
                    f.delete();
                }
            }
        }
        return directory.delete();
    }

    private static Properties paths(){
        Properties props = new Properties();

        String subHeaderPath = "TaskBuilder" + SEPARATOR + "src" + SEPARATOR + "main" + SEPARATOR + "haskell" + SEPARATOR;

        props.put("bin_path", System.getProperty("user.dir") + SEPARATOR + subHeaderPath);
        props.put("data_path", APPDATA + "data" + File.separator);

        return props;
    }
}
