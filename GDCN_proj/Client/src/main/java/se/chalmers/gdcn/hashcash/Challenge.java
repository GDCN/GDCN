package se.chalmers.gdcn.hashcash;

import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import javax.crypto.Mac;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Random;

/**
 * Created by Leif on 2014-03-29.
 *
 */
public class Challenge implements Serializable {
    public final HashCash.Purpose purpose;
    private final byte[] seed, mac;
    private final int difficulty;

    //TODO Remove this, it is only used for testing purposes!
    public Challenge(byte[] seed, int difficulty, Key key, byte[] mac) throws InvalidKeyException {
        this.purpose = HashCash.Purpose.NONE;
        this.seed = seed.clone();
        this.difficulty = difficulty;
        this.mac = mac.clone();
    }

    //TODO Remove this, it is only used for testing purposes!
    public byte[] getMAC() {
        return mac.clone();
    }

    /**
     * Creates a new anonymous Challenge.
     * @param seed The seed of the Challenge. Should not be reused!
     * @param difficulty The difficulty of the Challenge.
     * @param key The key which should be used to authenticate the solution. (Must be compatible with javax.crypto.Mac)
     */
    public Challenge(byte[] seed, int difficulty, Key key) throws InvalidKeyException {
        this(HashCash.Purpose.NONE,seed,difficulty,key);
    }

    /**
     * Creates a new identifiable Challenge.
     * @param purpose The purpose of the Challenge.
     * @param seed The seed of the Challenge. Should not be reused!
     * @param difficulty The difficulty of the Challenge.
     * @param key The key which should be used to authenticate the solution. (Must be compatible with javax.crypto.Mac)
     */
    public Challenge(HashCash.Purpose purpose, byte[] seed, int difficulty, Key key) throws InvalidKeyException {
        this.purpose = purpose;
        this.seed = seed.clone();
        this.difficulty = difficulty;
        this.mac = generateMAC(key);
    }

    /**
     * Gets the identity of the challenge
     * @return The identity if it exists, null otherwise.
     */
    public HashCash.Purpose getPurpose() {
        return purpose;
    }

    private byte[] generateMAC(Key key) throws InvalidKeyException {
        Mac macGen = null;
        try {
            macGen = Mac.getInstance(key.getAlgorithm());
            macGen.init(key);
            macGen.update(purpose.toString().getBytes("UTF-8"));
        } catch (InvalidKeyException e) {
            e.printStackTrace();
            //TODO Something went really wrong here, present it to the user in some way?
        } catch (NoSuchAlgorithmException e) {
            throw new InvalidKeyException("The key must be compatible with javax.crypto.Mac");
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            //TODO Tell the user that UTF-8 is needed.
        }

        macGen.update(seed);
        return macGen.doFinal(ByteBuffer.allocate(4).putInt(difficulty).array());
    }

    public boolean isAuthentic(Key key) throws InvalidKeyException {
        return Arrays.equals(this.mac, generateMAC(key));
    }

    /**
     * Solves the Challenge.
     * @return A solution such that this.solved(solution) == true
     */
    public Solution solve() {
        MessageDigest md = null;
        try {
            md = MessageDigest.getInstance(HashCash.HASH_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            //TODO Something went really wrong here, present it to the user in some way?
        }
        Random random = new Random();
        byte[] testToken;

        do {
            testToken = new byte[random.nextInt(32)];
            random.nextBytes(testToken);
        } while (!isCorrectToken(testToken));

        return new Solution(testToken,this);
    }

    @Override
    public String toString() {
        return "Challenge{\n" +
                "\tseed='" + seed + "',\n" +
                "\tdifficulty='" + difficulty + "',\n" +
                "\tmac='" + mac + '\n' +
                '}';
    }

    //TODO Should be made private after testing.
    public static byte[] hash(byte[] seed, byte[] token) {
        MessageDigest md = null;
        try {
            md = MessageDigest.getInstance(HashCash.HASH_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            //TODO Something went really wrong here, present it to the user in some way?
        }

        md.update(seed);
        md.update(token);
        return md.digest();
    }

    /**
     * Checks if the given token solves the challenge
     * @param token The token to be checked against the Challenge.
     * @return True if the token is a solution to the challenge, false otherwise.
     */
    public boolean isCorrectToken(byte[] token) {
        return checkZeros(hash(seed,token));
    }

    //TODO Should be made private after testing
    public boolean checkZeros(byte[] hash) {
        BitSet hashBits = BitSet.valueOf(hash);
        BitSet zeros = new BitSet(difficulty);

        zeros.clear();
        zeros.or(hashBits);

        return zeros.get(0,difficulty-1).isEmpty();
    }
}
