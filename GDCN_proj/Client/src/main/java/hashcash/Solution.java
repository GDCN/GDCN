package hashcash;

import java.io.Serializable;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;

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

    public boolean isValid(Key key) throws NoSuchAlgorithmException, InvalidKeyException {
        return isAuthentic(key) && isSolution();
    }

    private boolean isAuthentic(Key key) throws InvalidKeyException, NoSuchAlgorithmException {
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
