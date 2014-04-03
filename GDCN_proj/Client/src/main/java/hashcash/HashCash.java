package hashcash;

import network.WorkerID;

import java.io.UnsupportedEncodingException;
import java.security.*;

/**
 * Created by weeeeeew on 2014-03-31.
 */
public class HashCash {
    public final static String HASH_ALGORITHM = "SHA-1";
    
    public final int hardDifficulty, easyDifficulty;
    private final SecureRandom random;
    private final Key key;

    /**
     * Creates a new HashCash-cookie instance with standard difficulties.
     * The supplied key is used to validate solutions.
     * @param key A key for MACing, must be compatible with javax.crypto.Mac. At least SHA-256 is recommended,
     *            which can be generated with KeyGenerator.getInstance("HmacSHA256").generateKey()
     */
    public HashCash(Key key) throws InvalidKeyException {
        this(key,20,30); //TODO Insert real numbers for easy and hard difficulties.
    }

    /**
     * Creates a new HashCash-cookie instance with custom difficulties.
     * The supplied key is used to validate solutions.
     * @param key A key for MACing, must be compatible with javax.crypto.Mac. At least SHA-256 is recommended,
     *            which can be generated with KeyGenerator.getInstance("HmacSHA256").generateKey()
     * @param easy The difficulty of easy challenges.
     * @param hard The difficulty of hard challenges.
     */
    public HashCash(Key key, int easy, int hard) {
        this.key = key;
        hardDifficulty = hard;
        easyDifficulty = easy;
        random = new SecureRandom();
    }

    public Challenge generateChallenge(String seed, int difficulty) {
        try {
            return new Challenge(randomHash(seed), difficulty, key);
        } catch (InvalidKeyException e) {
            e.printStackTrace();
            //TODO Tell the user that the key supplied to the HashCash is invalid.
            return null;
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            //TODO Tell the user that UTF-8 is needed.
            return null;
        }
    }

    public Challenge generateChallenge(String purpose, String seed, int difficulty) {
        try {
            return new Challenge(purpose, randomHash(seed), difficulty, key);
        } catch (InvalidKeyException e) {
            e.printStackTrace();
            //TODO Tell the user that the key supplied to the HashCash is invalid.
            return null;
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            //TODO Tell the user that UTF-8 is needed.
            return null;
        }
    }

    public Challenge generateEasyChallenge(String jobOwner, String worker, String task) {
        return generateChallenge(jobOwner + worker + task, easyDifficulty);
    }

    public Challenge generateEasyChallenge(String purpose, String jobOwner, String worker, String task) {
        return generateChallenge(purpose, jobOwner + worker + task, easyDifficulty);
    }

    public Challenge generateHardChallenge(String jobOwner, String worker) {
        return generateChallenge(jobOwner + worker, hardDifficulty);
    }

    public Challenge generateHardChallenge(String purpose, String jobOwner, String worker) {
        return generateChallenge(purpose, jobOwner + worker, hardDifficulty);
    }

    public Challenge generateRegistrationChallenge(WorkerID jobOwner, WorkerID worker) {
        return generateChallenge("REGISTER", jobOwner.toString() + worker.toString(), hardDifficulty);
    }

    public Challenge generateAuthenticationChallenge(WorkerID jobOwner, WorkerID worker) {
        return generateChallenge("AUTHENTICATE", jobOwner.toString() + worker.toString(), easyDifficulty);
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