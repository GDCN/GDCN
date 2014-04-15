package se.chalmers.gdcn.network;

import java.io.*;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Created by HalfLeif on 2014-03-06.
 */
public class Box implements Serializable{
    private final byte[] digest;
    private final byte[] data;

    /**
     * @param data Data of box
     * @param digest Digest of data
     */
    private Box(byte[] data, byte[] digest) {
        this.digest = digest;
        this.data = data;
    }

    private static boolean same(byte[] a, byte[] b){

        if(a == b){
            return true;
        }
        if(a==null ^ b==null){
            return false;
        }
        if(a.length != b.length){
            return false;
        }
        for(int i=0; i<a.length; ++i){
            if(a[i] != b[i]){
                return false;
            }
        }
        return true;
    }

    /**
     * Outputs some arbitrary data to file
     * @param file File write in
     */
    public void toFile(File file, byte[] expectedDigest) throws IOException {
        //TODO fix: expectedDigest is unknown?
        if(! same(expectedDigest, digest)){
            throw new IllegalStateException("Not same digest!");
        }

        BufferedOutputStream outputStream = null;
        try {
            outputStream = new BufferedOutputStream(new FileOutputStream(file));
            outputStream.write(data);
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
     * TODO make safe: possibly not send digest together with message: too easy to crack
     * @param file
     * @return
     * @throws IOException
     */
    public static Box fromFile(File file) throws IOException {

        InputStream inputStream = null;

        try {
            //TODO Possibly use MD5 instead, if SHA-1 is too slow
            MessageDigest digest = MessageDigest.getInstance("SHA-1");

            inputStream = new BufferedInputStream(new FileInputStream(file));
            DigestInputStream digestInputStream = new DigestInputStream(inputStream, digest);

            byte[] data = new byte[(int) file.length()];

            digestInputStream.read(data);

            byte[] digestArray = digest.digest();

            return new Box(data, digestArray);

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
