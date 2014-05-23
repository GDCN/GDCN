package se.chalmers.gdcn.hashcash;

import se.chalmers.gdcn.network.WorkerID;

import javax.crypto.SecretKey;
import java.io.UnsupportedEncodingException;
import java.security.*;

/**
 * Created by weeeeeew on 2014-03-31.
 */
public class HashCash {
    public final static String HASH_ALGORITHM = "SHA-1";
    
    public final int hardDifficulty, easyDifficulty;
    private final SecretKey key;

    public static enum Purpose { REG, AUTH, NONE }

    /**
     * Creates a new HashCash-cookie instance with standard difficulties.
     * The supplied key is used to validate solutions.
     * @param key A key for MACing, must be compatible with javax.crypto.Mac. At least SHA-256 is recommended,
     *            which can be generated with KeyGenerator.getInstance("HmacSHA256").generateKey()
     */
    public HashCash(SecretKey key) throws InvalidKeyException {
        this(key,15,20);
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
    }

    /**
     * Generates a new Challenge without a purpose.
     * @param seed The seed of the challenge.
     * @param difficulty The difficulty of the challenge.
     * @return The challenge.
     */
    public Challenge generateChallenge(String seed, int difficulty) {
        return generateChallenge(Purpose.NONE, seed, difficulty);
    }

    /**
     * Generates a new Challenge with a purpose.
     * @param purpose The purpose of the challenge.
     * @param seed The seed of the challenge.
     * @param difficulty The difficulty of the challenge.
     * @return The challenge.
     */
    public Challenge generateChallenge(Purpose purpose, String seed, int difficulty) {
        try {
            return new Challenge(purpose, hash(seed), difficulty, key);
        } catch (InvalidKeyException e) {
            e.printStackTrace();
            //TODO Tell the user that the key supplied to the HashCash is invalid.
            return null;
        }
    }

    /**
     * Generates a hard challenge for registration purposes.
     * @param jobOwner The issuer of the challenge.
     * @param worker The worker that wants to register.
     * @param score The worker's current score.
     * @return The challenge.
     */
    public Challenge generateRegistrationChallenge(WorkerID jobOwner, WorkerID worker, int score) {
        String seed = jobOwner.toString() + worker + score;

        return generateChallenge(Purpose.REG, seed, hardDifficulty);
    }

    /**
     * Generates an easy challenge for authentication purposes.
     * @param jobOwner The issuer of the challenge.
     * @param worker The worker that wants to register.
     * @param score The worker's current score.
     * @return
     */
    public Challenge generateAuthenticationChallenge(WorkerID jobOwner, WorkerID worker, int score) {
        String seed = jobOwner.toString() + worker + score;

        return generateChallenge(Purpose.AUTH, seed, easyDifficulty);
    }

    /**
     * Checks the validity and authenticity of a solution.
     * @param solution The solution to check.
     * @param jobOwner The issuer of the challenge.
     * @param worker The worker that claims to have solved a challenge.
     * @param score The worker's current score.
     * @return True if the worker solved the solution, false otherwise.
     */
    public boolean validateSolution(Solution solution, WorkerID jobOwner, WorkerID worker, int score) throws InvalidKeyException {
        byte[] seed = hash(jobOwner.toString() + worker + score);

        return solution.isValid(key,seed);
    }

    private static byte[] hash(String message) {
        MessageDigest md = null;
        try {
            md = MessageDigest.getInstance(HASH_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            //TODO Something went really wrong here, present it to the user in some way?
        }

        try {
            md.update(message.getBytes("UTF-8"));
        } catch (UnsupportedEncodingException e) {
            System.out.println("HashCash: UTF-8 is required! Things will fail now.");
        }
        return md.digest();
    }
}