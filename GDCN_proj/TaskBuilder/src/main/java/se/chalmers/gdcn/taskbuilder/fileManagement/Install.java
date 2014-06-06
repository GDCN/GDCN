package se.chalmers.gdcn.taskbuilder.fileManagement;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import se.chalmers.gdcn.taskbuilder.ExitFailureException;

import java.io.*;
import java.net.URL;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by HalfLeif on 2014-03-04.
 *
 * Creates directories and files for application data in ~/.gdcn
 */
public class Install {

    private static final String SEPARATOR = File.separator;

    public static final String APPDATA = System.getProperty("user.home") + SEPARATOR + ".gdcn" + SEPARATOR;
    public static final String PATH_DATA = APPDATA + "pathdata.prop";
    public static final String HEADER_NAME = "Header.hs";
    public static final String HPKG_NAME = "gdcn-trusted";
    public static final String BOOTSTRAP_NODE_NAME = "bootstrapnodes";

    public static final String LIB_DIR = APPDATA + "lib";
    public static final String HDB_DIR = APPDATA + "hdb.conf.d";

    public static final String STD_BIN = APPDATA + "bin" + SEPARATOR;
    public static final String HASKELL_SUBDIR = "haskell";
    public static final String HEADER_LOCATION = STD_BIN+HASKELL_SUBDIR+SEPARATOR;

    //private constructor
    private Install(){
    }

    /**
     * Creates directory for application data. Creates file containing important paths used by application.
     * Must be run from GDCN_proj/ directory.
     */
    public static void install(){
        File rootPath = new File(APPDATA);
        rootPath.mkdirs();

        File pathDataFile = new File(PATH_DATA);
        Properties pathData = null;

        OutputStream outputStream = null;
        try {
            outputStream = new BufferedOutputStream(new FileOutputStream(pathDataFile));

            pathData = paths();
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

        if (pathData != null) {
            installHaskellLibraries();
        }

        writeBootstrapNodes();
    }

    /**
     * Deletes entire directory for application data.
     *
     * @return true iff uninstalled correctly
     */
    public static boolean uninstall(){
        File rootPath = new File(APPDATA);
        return deleteContents(rootPath);
    }

    /**
     * Deletes entire directory with contents. Use with care!
     * Is currently used in {@link Install#uninstall()} and {@link PathManager#deleteBinaries()} etc.
     *
     * http://stackoverflow.com/questions/7768071/how-to-delete-folder-content-in-java
     * @param directory Folder to delete
     * @return true if the directory was deleted, false otherwise
     */
    static boolean deleteContents(File directory){
        if(!directory.exists()){
            return false;
        }
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

    /**
     * Creates Property object containing paths critical for the program to work on tasks
     * @return standard paths
     */
    private static Properties paths(){
        Properties props = new Properties();

        props.put("bin_path", HEADER_LOCATION);
        props.put("data_path", APPDATA + "data" + File.separator);
        props.put("job_path", APPDATA + "jobs" + File.separator);
        props.put("settings_path", APPDATA + "settings" + File.separator);

        return props;
    }

    private static void installHaskellLibraries() {
        final File targetHaskellDir = new File(STD_BIN);

        try {
            Install obj = new Install();
            URL location = obj.getClass().getProtectionDomain().getCodeSource().getLocation();

            if(location.getFile().endsWith(".jar")){
                System.out.println("Location URL: "+location);

                File jar = new File(location.toURI());
                JarExtractor.extract(jar, HASKELL_SUBDIR, targetHaskellDir);
            } else {
                //If executed from within IDE
                System.out.println("Couldn't locate jar. Assumes is in source folder, attempt copy directly...");

                String subHeaderPath = "TaskBuilder" + SEPARATOR + "src" + SEPARATOR + "main" + SEPARATOR + "haskell" + SEPARATOR;
                String path = System.getProperty("user.dir") + SEPARATOR + subHeaderPath;
                System.out.println("Path: "+path);

                File haskellSourceDir = new File(path);
                FileUtils.copyDirectory(haskellSourceDir, new File(HEADER_LOCATION));
            }


        } catch (Exception e) {
            e.printStackTrace();
        }

        File buildDir = new File(HEADER_LOCATION + SEPARATOR + HPKG_NAME);
        System.out.println("Build dir: "+buildDir);

        String[] dbCmd = {"ghc-pkg", "init", HDB_DIR};
        String[] libCmd = {"runhaskell", "Setup", LIB_DIR, HDB_DIR};

        try {
            Process makeDb = new ProcessBuilder(dbCmd).start();

            if (makeDb.waitFor() != 0) {
                StringWriter writer = new StringWriter();
                IOUtils.copy(makeDb.getErrorStream(), writer, null);

                Pattern pattern = Pattern.compile(".*already exists.*", Pattern.DOTALL);
                Matcher matcher = pattern.matcher(writer.toString().toLowerCase());
                if (!matcher.matches()) {
                    throw new ExitFailureException(writer.toString());
                }
            }


            Process makeLib = new ProcessBuilder(libCmd).directory(buildDir).start();

            if (makeLib.waitFor() != 0) {
                StringWriter writer = new StringWriter();
                IOUtils.copy(makeDb.getErrorStream(), writer, null);
                throw new ExitFailureException(writer.toString());
            }
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        catch (InterruptedException e) {
            e.printStackTrace();
        }
        catch (ExitFailureException e) {
            e.printStackTrace();
        }
    }

    private static void writeBootstrapNodes() {
        String host = "narrens.olf.sgsnet.se";
        int port = 4001;

        String s = paths().getProperty("settings_path") + BOOTSTRAP_NODE_NAME;

        File bootstrapFile = new File(s);
        bootstrapFile.getParentFile().mkdirs();

        try {
            BufferedWriter outputStream = new BufferedWriter(new FileWriter(bootstrapFile));

            try {
                outputStream.write(host + " " + port + "\n");
            } finally {
                outputStream.close();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
