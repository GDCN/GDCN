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
    private final byte[] seed, mac;
    private final int difficulty;


    /**
     * Creates a new Challenge.
     * @param seed The seed of the Challenge. Should not be reused!
     * @param difficulty The difficulty of the Challenge.
     */
    public Challenge(byte[] seed, int difficulty, Key key) throws InvalidKeyException {
        Mac macgen = null;
        try {
            macgen = Mac.getInstance("SHA-1");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            //TODO Something went really wrong here, present it to the user in some way?
        }

        this.seed = seed;
        this.difficulty = difficulty;

        macgen.init(key);
        macgen.update(seed);
        this.mac = macgen.doFinal(ByteBuffer.allocate(4).putInt(difficulty).array());
    }

    public boolean isAuthentic(Key key) throws InvalidKeyException, NoSuchAlgorithmException {
        Mac macgen = Mac.getInstance("SHA-1");
        macgen.init(key);
        macgen.update(seed);
        byte[] mac = macgen.doFinal(ByteBuffer.allocate(4).putInt(difficulty).array());

        return Arrays.equals(this.mac, mac);
    }

    /**
     * Solves the Challenge.
     * @return A solution such that this.solved(solution) == true
     */
    public Solution solve() {
        MessageDigest md = null;
        try {
            md = MessageDigest.getInstance("SHA-1");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            //TODO Something went really wrong here, present it to the user in some way?
        }
        Random random = new Random();
        byte[] rest;

        do {
            rest = new byte[random.nextInt(32)];
            random.nextBytes(rest);
            md.update(seed);
            md.update(rest);
        } while (!checkZeros(md.digest(), difficulty));

        return new Solution(rest,this);
    }

    @Override
    public String toString() {
        return "Challenge{\n" +
                "\tseed='" + seed + "',\n" +
                "\tdifficulty='" + difficulty + "',\n" +
                "\tmac='" + mac + '\n' +
                '}';
    }

    /**
     * Checks if the given token solves the challenge
     * @param token
     * @return True if the token is a solution to the challenge, false otherwise.
     */
    public boolean isCorrectToken(byte[] token) {
        MessageDigest md = null;
        try {
            md = MessageDigest.getInstance("SHA-1");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            //TODO Something went really wrong here, present it to the user in some way?
        }

        md.update(seed);
        md.update(token);

        return checkZeros(md.digest(), difficulty);
    }

    private static boolean checkZeros(byte[] sha, int difficulty) {
        int i;
        for (i = 0; i < difficulty/8; i++) {
            if (sha[i] != 0) {
                return false;
            }
        }
        byte b = 1;
        for (int j = 0; j < difficulty%8; j++) {
            if ((sha[i] & b) != 0) {
                return false;
            }
        }
        return true;
    }
}
