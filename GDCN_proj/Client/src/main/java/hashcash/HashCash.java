package hashcash;

import java.io.UnsupportedEncodingException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Random;

/**
 * Created by weeeeeew on 2014-03-31.
 */
public class HashCash {
    private final Random random;
    private final Key key;

    public HashCash(Key key) {
        random = new Random();
        this.key = key;
    }

    public Challenge generateChallenge(String seed, int difficulty) throws UnsupportedEncodingException {
        MessageDigest md = null;
        try {
            md = MessageDigest.getInstance("SHA-1");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            //TODO Something went really wrong here, present it to the user in some way?
        }
        byte[] randomBytes = new byte[random.nextInt(32)];

        md.update(seed.getBytes("UTF-8"));
        md.update(randomBytes);

        try {
            return new Challenge(md.digest(), difficulty, key);
        } catch (InvalidKeyException e) {
            e.printStackTrace();
            //TODO Tell the user that the key supplied to the HashCash is invalid.
            return null;
        }
    }

    public Challenge generateEasyChallenge(String jobOwner, String worker, String task) throws UnsupportedEncodingException, NoSuchAlgorithmException, InvalidKeyException {
        StringBuilder sb = new StringBuilder(jobOwner);
        sb.append(worker);
        sb.append(task);

        return generateChallenge(sb.toString(), 20);
        //TODO insert real difficulty
    }

    public Challenge generateHardChallenge(String jobOwner, String worker) throws UnsupportedEncodingException, NoSuchAlgorithmException, InvalidKeyException {
        StringBuilder sb = new StringBuilder(jobOwner);
        sb.append(worker);

        return generateChallenge(sb.toString(), 100);
        //TODO insert real difficulty
    }
}