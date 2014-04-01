package hashcash;

import java.io.UnsupportedEncodingException;
import java.security.*;

/**
 * Created by weeeeeew on 2014-03-31.
 */
public class HashCash {
    public final static String HASH_ALGORITHM = "SHA-1";

    private final SecureRandom random;
    private final Key key;

    /**
     * Creates a new HashCash-cookie instance. The supplied key is used to validate solutions.
     * @param key A key for MACing, must be compatible with javax.crypto.Mac. At least SHA-256 is recommended,
     *            which can be generated with KeyGenerator.getInstance("HmacSHA256").generateKey()
     */
    public HashCash(Key key) throws InvalidKeyException {
        this.key = key;
        random = new SecureRandom();
    }

    public Challenge generateChallenge(String seed, int difficulty) throws UnsupportedEncodingException {
        try {
            return new Challenge(randomHash(seed), difficulty, key);
        } catch (InvalidKeyException e) {
            e.printStackTrace();
            //TODO Tell the user that the key supplied to the HashCash is invalid.
            return null;
        }
    }

    public Challenge generateChallenge(String id, String seed, int difficulty) throws UnsupportedEncodingException {
        try {
            return new Challenge(id.getBytes("UTF-8"), randomHash(seed), difficulty, key);
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

    public Challenge generateEasyChallenge(String challengeId, String jobOwner, String worker, String task) throws UnsupportedEncodingException, NoSuchAlgorithmException, InvalidKeyException {
        StringBuilder sb = new StringBuilder(jobOwner);
        sb.append(worker);
        sb.append(task);

        return generateChallenge(challengeId, sb.toString(), 20);
        //TODO insert real difficulty
    }

    public Challenge generateHardChallenge(String jobOwner, String worker) throws UnsupportedEncodingException, NoSuchAlgorithmException, InvalidKeyException {
        StringBuilder sb = new StringBuilder(jobOwner);
        sb.append(worker);

        return generateChallenge(sb.toString(), 100);
        //TODO insert real difficulty
    }

    public Challenge generateHardChallenge(String challengeId, String jobOwner, String worker) throws UnsupportedEncodingException, NoSuchAlgorithmException, InvalidKeyException {
        StringBuilder sb = new StringBuilder(jobOwner);
        sb.append(worker);

        return generateChallenge(challengeId, sb.toString(), 100);
        //TODO insert real difficulty
    }

    private byte[] randomHash(String message) throws UnsupportedEncodingException {
        MessageDigest md = null;
        try {
            md = MessageDigest.getInstance(HASH_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            //TODO Something went really wrong here, present it to the user in some way?
        }

        byte[] randomBytes = new byte[random.nextInt(32)];
        md.update(message.getBytes("UTF-8"));
        md.update(randomBytes);
        return md.digest();
    }
}