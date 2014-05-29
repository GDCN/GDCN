package se.chalmers.gdcn.taskbuilder.fileManagement;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * Created by HalfLeif on 2014-05-29.
 */
public class JarExtractor {

    /**
     * Extracts a subdirectory of a jar file to a custom location.
     *
     * @param jar jar file to extract from
     * @param subDir subDir of jar to extract
     * @param targetDir Target directory to extract to
     */
    public static void extract(File jar, String subDir, File targetDir) throws IOException {
        JarFile jarFile = new JarFile(jar);

        targetDir.mkdirs();
        System.out.println("Target dir: "+targetDir);
//        System.out.println("exist? " + targetDir.exists());

        Enumeration<JarEntry> entries = jarFile.entries();
        while (entries.hasMoreElements()){
            JarEntry entry = entries.nextElement();
            String name = entry.getName();

            if(! subDir.equals(name.split(File.separator)[0])){
                continue;
            }
//            System.out.println("Entry: "+name);
            File targetFile = new File(targetDir + File.separator + name);

            if(entry.isDirectory()){
                System.out.println("dir: "+name);
                targetFile.mkdir();
                continue;
            } else {
                System.out.println("file: "+name);
            }

            InputStream inputStream = jarFile.getInputStream(entry);
            FileOutputStream fileOutputStream = new FileOutputStream(targetFile);
            while(inputStream.available() > 0){
                fileOutputStream.write(inputStream.read());
            }

            inputStream.close();
            fileOutputStream.close();
        }

        System.out.println("\nExtraction complete.");
    }

}
