import hashcash.*;
import org.testng.annotations.Test;

import javax.crypto.KeyGenerator;
import java.math.BigInteger;
import java.security.Key;
import java.util.Random;

/**
 * Created by Leif on 2014-03-29.
 */
public class HashCashTest {
    private final Key key;
    private final HashCash hc;
    private final Random random;

    public HashCashTest() throws Exception {
        key = KeyGenerator.getInstance("HmacSHA256").generateKey();
        hc = new HashCash(key);
        random = new Random();
    }

    @Test
    public void testRandomSolution() throws Exception {
        Challenge challenge = hc.generateChallenge(randomString(),random.nextInt(100));

        assert challenge.solve().isValid(key);
    }

    @Test
    public void testRandomIdSolution() throws Exception {
        Challenge challenge = hc.generateChallenge(randomString(),randomString(),random.nextInt(100));

        assert challenge.solve().isValid(key);
    }
    
    @Test
    public void testEasySolution() throws Exception {
        Challenge challenge = hc.generateEasyChallenge(randomString(),randomString(),randomString());

        assert challenge.solve().isValid(key);
    }

    @Test
    public void testEasyIdSolution() throws Exception {
        String id = randomString();
        Challenge challenge = hc.generateEasyChallenge(id,randomString(),randomString(),randomString());
        Solution solution = challenge.solve();

        assert solution.isValid(key) && solution.getId().equals(id.getBytes("UTF-8"));
    }

    @Test
    public void testHardSolution() throws Exception {
        Challenge challenge = hc.generateHardChallenge(randomString(),randomString());

        assert challenge.solve().isValid(key);
    }

    @Test
    public void testHardIdSolution() throws Exception {
        String id = randomString();
        Challenge challenge = hc.generateHardChallenge(id,randomString(),randomString());
        Solution solution = challenge.solve();

        assert solution.isValid(key) && solution.getId().equals(id.getBytes("UTF-8"));
    }

    @Test
    public void testInvalidSolution() throws Exception {
        Challenge challenge = hc.generateChallenge(randomString(), 2);
        Solution invalidSolution = new Solution(randomString().getBytes("UTF-8"),challenge);

        assert !invalidSolution.isValid(key);
    }
/*
    @Test
    public void testNonAuthenticSolution() throws Exception {
        byte[] seed = randomString().getBytes("UTF-8");
        Challenge actualChallenge = new Challenge(seed,20,key);
        Challenge falseChallenge = new Challenge(seed,10,key);


    }
*/
    private String randomString() {
        return new BigInteger(130, random).toString(32);
    }
}
