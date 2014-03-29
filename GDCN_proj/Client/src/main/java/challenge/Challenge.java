package challenge;

import java.io.Serializable;
import java.util.Random;

/**
 * Created by Leif on 2014-03-29.
 */
public class Challenge implements Serializable{

    private final String key;

    private final Random random = new Random();

    private Challenge() {
        key = ""+random.nextLong();
    }

    public boolean isSolution(Solution solution){
        //TODO check if is solution
        return key.equals(solution.getKey());
    }

    public static Challenge generate(){
        //TODO generate new Challenge
        return new Challenge();
    }

    public String getKey() {
        return key;
    }
}
