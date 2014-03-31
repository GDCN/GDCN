package challenge;

import java.io.Serializable;
import java.util.Random;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Created by Leif on 2014-03-29.
 */
public class Solution implements Serializable{

    private final String key;

    private Solution(String key) {
        this.key = key;
    }

    public static Solution solve(Challenge challenge) throws NoSuchAlgorithmException{
        MessageDigest md = MessageDigest.getInstance("SHA-1");
        Random random = new Random();
        byte[] rest;

        do {
            rest = new byte[random.nextInt(32)];
            random.nextBytes(rest);
            //md.update(key);
            md.update(rest);
        } while (checkZeros(md.digest(), 20)); //TODO Replace 20 with actual difficulty

        return new Solution(challenge.getKey());
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

    public String getKey() {
        return key;
    }

    @Override
    public String toString() {
        return "Solution{" +
                "key='" + key + '\'' +
                '}';
    }
}
