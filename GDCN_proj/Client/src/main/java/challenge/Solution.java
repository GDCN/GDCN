package challenge;

import java.io.Serializable;

/**
 * Created by Leif on 2014-03-29.
 */
public class Solution implements Serializable{

    private final String key;

    private Solution(String key) {
        this.key = key;
    }

    public static Solution solve(Challenge challenge){
        return new Solution(challenge.getKey());
    }

    public String getKey() {
        return key;
    }
}
