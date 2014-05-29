package se.chalmers.gdcn.taskbuilder.fileManagement;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import se.chalmers.gdcn.taskbuilder.ExitFailureException;

import java.io.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.CodeSource;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarInputStream;
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

    //private constructor
    private Install(){
    }

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
            installHaskellLibraries(pathData.getProperty("bin_path"));
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
     * @return
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
     * @return
     */
    private static Properties paths(){
        Properties props = new Properties();

        String subHeaderPath = "TaskBuilder" + SEPARATOR + "src" + SEPARATOR + "main" + SEPARATOR + "haskell" + SEPARATOR;

        props.put("bin_path", System.getProperty("user.dir") + SEPARATOR + subHeaderPath);
        props.put("data_path", APPDATA + "data" + File.separator);
        props.put("job_path", APPDATA + "jobs" + File.separator);
        props.put("settings_path", APPDATA + "settings" + File.separator);

        return props;
    }

    /**
     * Extracts haskell dirs in jar file.
     * @return {@link java.io.File} to extracted directory
     */
    private static File extractHaskellDirs(){
        Install obj = new Install();
        URL resource = obj.getClass().getResource("/haskell");
        System.out.println("HaskellFolder: " + resource);

        File r = null;
        try {
            r = new File(resource.toURI());
            System.out.println("URI successful: "+r.getAbsolutePath());
        } catch (Exception e) {
            //URISyntaxException?
            e.printStackTrace();

            try {
                r = new File(resource.getPath());
                System.out.println("String successful: "+r.getAbsolutePath());
            } catch (Exception e1) {
                e1.printStackTrace();
            }
        }

        File haskellDir = new File(APPDATA + SEPARATOR + "tempHaskell");
        haskellDir.mkdirs();
//        InputStream resourceStream = obj.getClass().getResourceAsStream("/haskell");

        try {
            FileUtils.copyDirectory(r, haskellDir, false);
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }

        return haskellDir;
    }

    private static boolean extractHaskellDirs2(File targetDir){
        targetDir.mkdirs();

//        Install obj = new Install();
        URL resource = Thread.currentThread().getClass().getResource("/haskell");
        System.out.println("HaskellFolder URL: " + resource);

        URI res = null;
        try {
             res = resource.toURI();
        } catch (URISyntaxException e) {
            e.printStackTrace();
            return false;
        }

        System.out.println("HaskellFolder URI: " + res);

        Map<String, String> env = new HashMap<>();
        try {
//            FileSystem fileSystem = FileSystems.newFileSystem(res, env);
            FileSystem fileSystem = FileSystems.newFileSystem(res, env);
//            Path path = fileSystem.getPath(".");
            Path path = Paths.get(res);
            System.out.println("Paths: "+Paths.get(res));
            System.out.println("FileSystem: "+fileSystem.getPath("/haskell"));

        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }

//        InputStream resourceStream = obj.getClass().getResourceAsStream("/haskell");
//        FileInputStream fileInputStream = new FileInputStream(resourceStream);

//        try {
//            FileUtils.copyDirectory(r, haskellDir, false);
//        } catch (IOException e) {
//            e.printStackTrace();
//            return null;
//        }

        //todo fix
        return false;
    }

    private static void extractHaskellDirs5(){
        Install obj = new Install();
        CodeSource codeSource = obj.getClass().getProtectionDomain().getCodeSource();
        URL location = codeSource.getLocation();
        System.out.println("URL: "+ location);

        try {
            JarFile jarFile = new JarFile(location.getFile());

            JarEntry haskellEntry = jarFile.getJarEntry("haskell");
            if(haskellEntry == null){
                System.out.println("HaskellEntry null!!!");
            }

            File targetHaskellDir = new File(APPDATA + SEPARATOR + "tempHaskell");
            File targetFile = new File(targetHaskellDir + SEPARATOR + haskellEntry.getName());


            InputStream inputStream = jarFile.getInputStream(haskellEntry);
            FileOutputStream fileOutputStream = new FileOutputStream(targetFile);
            while(inputStream.available() > 0){
                fileOutputStream.write(inputStream.read());
            }

            inputStream.close();
            fileOutputStream.close();

            //            Enumeration<JarEntry> entries = jarFile.entries();
//            while (entries.hasMoreElements()){
//                JarEntry entry = entries.nextElement();
//            }

        } catch (IOException e) {
            e.printStackTrace();
        } catch (Exception e){
            e.printStackTrace();
        }
    }

    private static void extractHaskellDirs4(){
        File targetHaskellDir = new File(APPDATA + SEPARATOR + "tempHaskell");
        Install obj = new Install();
        CodeSource codeSource = obj.getClass().getProtectionDomain().getCodeSource();
        URL location = codeSource.getLocation();

        System.out.println("Location: "+location);

        JarInputStream jarInputStream = null;
        try{
            jarInputStream = new JarInputStream(new FileInputStream(location.getFile()));


        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (jarInputStream != null) {
                try {
                    jarInputStream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

//        ZipInputStream zipInputStream = new ZipInputStream(location.getFile());


    }

    private static void extractHaskellDirs3(){
        File targetHaskellDir = new File(APPDATA + SEPARATOR + "tempHaskell");

//        URL resource = Thread.currentThread().getContextClassLoader().getResource("haskell");
//        System.out.println("URL resource: "+resource);

        InputStream resourceStream = Thread.currentThread().getContextClassLoader().getResourceAsStream("haskell");

    }

    private static void installHaskellLibraries(String bin_path) {
        File targetHaskellDir = new File(APPDATA + SEPARATOR + "tempHaskell");

        extractHaskellDirs5();

//        if( !extractHaskellDirs2(targetHaskellDir) ){
//            File sourceDir = new File(bin_path + HPKG_NAME);
//            try {
//                FileUtils.copyDirectory(sourceDir, targetHaskellDir);
//            } catch (IOException e) {
//                e.printStackTrace();
//            }
//        }
        System.out.println("Extracted location: "+targetHaskellDir);

        //buildir: .../GDCN_proj/TaskBuilder/.../haskell/gdcn-trusted/
        File buildDir = new File(targetHaskellDir.getAbsolutePath() + HPKG_NAME);
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
