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
    private final Key key1, key2;
    private final HashCash hc;
    private final Random random;

    public HashCashTest() throws Exception {
        KeyGenerator keygen = KeyGenerator.getInstance("HmacSHA256");
        key1 = keygen.generateKey();
        key2 = keygen.generateKey();
        hc = new HashCash(key1);
        random = new Random();
    }

    @Test
    public void testRandomSolution() throws Exception {
        Challenge challenge = hc.generateChallenge(randomString(),random.nextInt(100));

        assert challenge.solve().isValid(key1);
    }

    @Test
    public void testRandomIdSolution() throws Exception {
        Challenge challenge = hc.generateChallenge(randomString(),randomString(),random.nextInt(100));

        assert challenge.solve().isValid(key1);
    }
    
    @Test
    public void testEasySolution() throws Exception {
        Challenge challenge = hc.generateEasyChallenge(randomString(),randomString(),randomString());

        assert challenge.solve().isValid(key1);
    }

    @Test
    public void testEasyIdSolution() throws Exception {
        String id = randomString();
        Challenge challenge = hc.generateEasyChallenge(id,randomString(),randomString(),randomString());
        Solution solution = challenge.solve();

        assert solution.isValid(key1) && solution.getId().equals(id.getBytes("UTF-8"));
    }

    @Test
    public void testHardSolution() throws Exception {
        Challenge challenge = hc.generateHardChallenge(randomString(),randomString());

        assert challenge.solve().isValid(key1);
    }

    @Test
    public void testHardIdSolution() throws Exception {
        String id = randomString();
        Challenge challenge = hc.generateHardChallenge(id,randomString(),randomString());
        Solution solution = challenge.solve();

        assert solution.isValid(key1) && solution.getId().equals(id.getBytes("UTF-8"));
    }

    @Test
    public void testWrongSolution() throws Exception {
        Challenge challenge = hc.generateChallenge(randomString(), 2);
        Solution invalidSolution = new Solution(randomString().getBytes("UTF-8"),challenge);

        assert !invalidSolution.isValid(key1);
    }

    @Test
    public void testTooEasySolution() throws Exception {
        byte[] seed = randomString().getBytes("UTF-8");
        Challenge hardChallenge = new Challenge(seed,20, key1);
        Challenge easyChallenge = new Challenge(seed,10, key1);
        Solution easySolution = easyChallenge.solve();
        Solution tooEasySolution = new Solution(easySolution.getToken(),hardChallenge);

        assert !tooEasySolution.isValid(key1);
    }

    @Test
    public void testWrongKey() throws Exception {
        Challenge challenge = new Challenge(randomString().getBytes("UTF-8"),2, key1);
        Solution solution = challenge.solve();

        assert !solution.isValid(key2);
    }

    @Test
    public void testWrongMAC() throws Exception {
        byte[] seed = randomString().getBytes("UTF-8");
        Challenge authenticChallenge = new Challenge(seed,2,key1);
        byte[] mac = authenticChallenge.getMAC();
        Challenge nonAuthenticChallenge = new Challenge(seed,3,key1,mac);

        assert !nonAuthenticChallenge.isAuthentic(key1);
    }

    private String randomString() {
        return new BigInteger(130, random).toString(32);
    }
}
