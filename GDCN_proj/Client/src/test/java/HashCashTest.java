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

    public void testEasyIdSolution() throws Exception {
        String id = randomString();
        Challenge challenge = hc.generateEasyChallenge(id,randomString(),randomString(),randomString());
        Solution solution = challenge.solve();

        assert solution.isValid(key) && solution.getId().equals(id.getBytes("UTF-8"));
    }
    
    public void testHardSolution() throws Exception {
        Challenge challenge = hc.generateHardChallenge(randomString(),randomString());

        assert challenge.solve().isValid(key);
    }

    public void testHardIdSolution() throws Exception {
        String id = randomString();
        Challenge challenge = hc.generateHardChallenge(id,randomString(),randomString());
        Solution solution = challenge.solve();

        assert solution.isValid(key) && solution.getId().equals(id.getBytes("UTF-8"));
    }

    private String randomString() {
        return new BigInteger(130, random).toString(32);
    }
}
