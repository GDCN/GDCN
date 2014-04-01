package hashcash;

import java.io.Serializable;
import java.nio.ByteBuffer;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import javax.crypto.Mac;
import java.util.Arrays;
import java.util.Random;

/**
 * Created by Leif on 2014-03-29.
 *
 */
public class Challenge implements Serializable {
    private final byte[] id, seed, mac;
    private final int difficulty;
    private final boolean identifiable;

    /**
     * Creates a new anonymous Challenge.
     * @param seed The seed of the Challenge. Should not be reused!
     * @param difficulty The difficulty of the Challenge.
     * @param key The key which should be used to authenticate the solution. (Must be compatible with javax.crypto.Mac)
     */
    public Challenge(byte[] seed, int difficulty, Key key) throws InvalidKeyException {
        this(new byte[]{-1},seed,difficulty,key);
    }

    /**
     * Creates a new identifiable Challenge.
     * @param id The identity of the Challenge.
     * @param seed The seed of the Challenge. Should not be reused!
     * @param difficulty The difficulty of the Challenge.
     * @param key The key which should be used to authenticate the solution. (Must be compatible with javax.crypto.Mac)
     */
    public Challenge(byte[] id, byte[] seed, int difficulty, Key key) throws InvalidKeyException {
        this.id = id.clone();
        this.seed = seed.clone();
        this.difficulty = difficulty;
        this.mac = generateMAC(key);
        this.identifiable = !(id.length == 1 && id[0] == -1);
    }

    /**
     * Gets the identity of the challenge
     * @return The identity if it exists, null otherwise.
     */
    public byte[] getId() {
        if(identifiable) {
            return id.clone();
        } else {
            return null;
        }
    }

    private byte[] generateMAC(Key key) throws InvalidKeyException {
        Mac macGen = null;
        try {
            macGen = Mac.getInstance(key.getAlgorithm());
            macGen.init(key);
        } catch (InvalidKeyException e) {
            e.printStackTrace();
            //TODO Something went really wrong here, present it to the user in some way?
        } catch (NoSuchAlgorithmException e) {
            throw new InvalidKeyException("The key must be compatible with javax.crypto.Mac");
        }

        macGen.update(id);
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

    private byte[] hash(byte[] seed, byte[] token) {
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
     * @param token
     * @return True if the token is a solution to the challenge, false otherwise.
     */
    public boolean isCorrectToken(byte[] token) {
        return checkZeros(hash(seed,token));
    }

    private boolean checkZeros(byte[] hash) {
        int i;
        for (i = 0; i < difficulty/8; i++) {
            if (hash[i] != 0) {
                return false;
            }
        }

        for (int j = 0; j < difficulty%8; j++) {
            if ((hash[i] & 2^j) != 0) {
                return false;
            }
	    b = b << 1;
        }
        return true;
    }
}
