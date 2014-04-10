package hashcash;

import network.WorkerID;

import javax.crypto.SecretKey;
import java.io.UnsupportedEncodingException;
import java.security.*;

/**
 * Created by weeeeeew on 2014-03-31.
 */
public class HashCash {
    public final static String HASH_ALGORITHM = "SHA-1";
    
    public final int hardDifficulty, easyDifficulty;
    private final SecureRandom random;
    private final SecretKey key;

    public static enum Purpose { REGISTER, AUTHENTICATE, NONE }

    /**
     * Creates a new HashCash-cookie instance with standard difficulties.
     * The supplied key is used to validate solutions.
     * @param key A key for MACing, must be compatible with javax.crypto.Mac. At least SHA-256 is recommended,
     *            which can be generated with KeyGenerator.getInstance("HmacSHA256").generateKey()
     */
    public HashCash(SecretKey key) throws InvalidKeyException {
        this(key,20,25);
        //TODO Insert real numbers for easy and hard difficulties.
        // 28-30 seems good for REG. Lowers this for debugging purposes.
        // perhaps 22-25 for AUTH
    }

    /**
     * Creates a new HashCash-cookie instance with custom difficulties.
     * The supplied key is used to validate solutions.
     * @param key A key for MACing, must be compatible with javax.crypto.Mac. At least SHA-256 is recommended,
     *            which can be generated with KeyGenerator.getInstance("HmacSHA256").generateKey()
     * @param easy The difficulty of easy challenges.
     * @param hard The difficulty of hard challenges.
     */
    public HashCash(SecretKey key, int easy, int hard) {
        this.key = key;
        hardDifficulty = hard;
        easyDifficulty = easy;
        random = new SecureRandom();
    }

    public Challenge generateChallenge(String seed, int difficulty) {
        return generateChallenge(Purpose.NONE, seed, difficulty);
    }

    public Challenge generateChallenge(Purpose purpose, String seed, int difficulty) {
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

    public Challenge generateRegistrationChallenge(WorkerID jobOwner, WorkerID worker) {
        return generateChallenge(Purpose.REGISTER, jobOwner.toString() + worker.toString(), hardDifficulty);
    }

    public Challenge generateAuthenticationChallenge(WorkerID jobOwner, WorkerID worker) {
        return generateChallenge(Purpose.AUTHENTICATE, jobOwner.toString() + worker.toString(), easyDifficulty);
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