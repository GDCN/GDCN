package replica.tester;

import net.tomp2p.storage.Data;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.List;

/**
 * Created by joakim on 4/2/14.
 */
public class ResultCompare {

    public boolean compareResults(List<byte[]> results) {
        byte[] prev = null;
        for (byte[] res : results) {
            byte[] curr = getDigest(res);
            if (prev != null && !Arrays.equals(curr, prev)) {
                return false;
            }
            prev = curr;
        }
        return true;
    }

    private byte[] getDigest(byte[] data) {
        try {
            MessageDigest messageDigest = MessageDigest.getInstance("SHA-1");
            return messageDigest.digest(data);
        }
        catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            //TODO Something is very wrong if this happens, maybe take some action?
        }
        return null;
    }

    // Opening data from file is already implemented in Uploader, with digest generation
    // (however it does not return the digest)
}
