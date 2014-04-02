//TODO Give it a package

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;

/**
 * Created by joakim on 4/2/14.
 */
public class ResultCompare {

    private final List<String> results;

    public ResultCompare(List<String> results) {
        this.results = results;
    }

    private byte[] getFileHash(String result) throws IOException, NoSuchAlgorithmException {
        File file = new File(result);
        DigestInputStream dis = new DigestInputStream(new FileInputStream(file), MessageDigest.getInstance("SHA-1"));
        for (int i = 0; i < file.length(); i++) {
            dis.read();
        }

        return dis.getMessageDigest().digest();
    }
}
