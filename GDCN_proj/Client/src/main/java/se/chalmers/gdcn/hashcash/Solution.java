package se.chalmers.gdcn.hashcash;

import java.io.Serializable;
import java.security.InvalidKeyException;
import java.security.Key;
import java.util.Arrays;

/**
 * Created by Leif on 2014-03-29.
 */
public class Solution implements Serializable {
    private final byte[] token;
    private final Challenge challenge;

    public Solution(byte[] token, Challenge challenge) {
        this.token = token;
        this.challenge = challenge;
    }

    public int getDifficulty() {
        return challenge.difficulty;
    }

    /**
     * Gets the purpose of the challenge this solution claims to solve.
     * @return The identity of the challenge if it exists, null otherwise.
     */
    public HashCash.Purpose getPurpose() {
        return challenge.purpose;
    }

    byte[] getToken() {
        return token.clone();
    }

    /**
     * Checks whether the solution is a valid solution to the challenge it claims to solve.
     * @param key The key used when creating the challenge.
     * @return True if the solution solves the challenge and is authentic.
     * @throws InvalidKeyException
     */
    public boolean isValid(Key key, byte[] seed) throws InvalidKeyException {
        return isValid(key) && Arrays.equals(seed,challenge.getSeed());
    }

    /**
     * Checks whether the solution is a valid solution to the challenge it claims to solve.
     * @param key The key used when creating the challenge.
     * @return True if the solution solves the challenge and is authentic.
     * @throws InvalidKeyException
     */
    public boolean isValid(Key key) throws InvalidKeyException {
        return isAuthentic(key) && isSolution();
    }

    private boolean isAuthentic(Key key) throws InvalidKeyException {
        return challenge.isAuthentic(key);
    }

    private boolean isSolution() {
        return challenge.isCorrectToken(token);
    }

    @Override
    public String toString() {
        return "Solution{\n" +
                "\ttoken='" + token + "',\n" +
                "\tchallenge=" + challenge + '\n' +
                '}';
    }
}
