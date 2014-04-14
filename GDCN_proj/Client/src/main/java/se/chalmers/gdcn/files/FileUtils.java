package se.chalmers.gdcn.files;

import java.io.*;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Created by HalfLeif on 2014-04-02.
 */
public class FileUtils {

    /**
     * Outputs some arbitrary data to file on disk
     *
     * @param file File to turn into byte[]
     * @param data contents of file
     */
    public static void toFile(File file, byte[] data){
        System.out.println("Attempt create file "+file.getAbsolutePath());
        File parent = file.getParentFile();
        parent.mkdirs();

        //TODO use Box class and do checksum, or shall we?
        BufferedOutputStream outputStream = null;
        try {
            outputStream = new BufferedOutputStream(new FileOutputStream(file));
            outputStream.write(data);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (outputStream != null) {
                try {
                    outputStream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Reads file on disk
     *
     * @param file File to read from
     * @return File contents in binary format
     * @throws IOException
     */
    public static byte[] fromFile(File file) throws IOException {

        InputStream inputStream = null;

        try {
            //TODO Possibly use MD5 instead, if SHA-1 is too slow
            MessageDigest digest = MessageDigest.getInstance("SHA-1");

            inputStream = new BufferedInputStream(new FileInputStream(file));
            DigestInputStream digestInputStream = new DigestInputStream(inputStream, digest);

            byte[] data = new byte[(int) file.length()];

            //TODO actually use digest
            digestInputStream.read(data);
            byte[] digestArray = digest.digest();

            return data;

        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } finally {
            if (inputStream != null) {
                inputStream.close();
            }
        }
        return null;
    }
}
