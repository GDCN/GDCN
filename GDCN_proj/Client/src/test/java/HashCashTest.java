import hashcash.*;
import network.WorkerID;
import org.testng.annotations.Test;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import java.math.BigInteger;
import java.security.KeyPairGenerator;
import java.util.BitSet;
import java.util.Random;

/**
 * Created by Leif on 2014-03-29.
 */
public class HashCashTest {
    private final SecretKey key1, key2;
    private final KeyPairGenerator keygen = KeyPairGenerator.getInstance("RSA");
    private final HashCash hc;
    private final Random random;

    public HashCashTest() throws Exception {
        KeyGenerator keygen = KeyGenerator.getInstance("HmacSHA256");
        key1 = keygen.generateKey();
        key2 = keygen.generateKey();
        hc = new HashCash(key1,20,22);
        random = new Random();
    }

    @Test
    public void testCheckZerosPos() {
        int difficulty = random.nextInt(100);
        Challenge challenge = hc.generateChallenge(randomString(),difficulty);
        BitSet hashBits = new BitSet(difficulty*2);

        hashBits.clear();
        hashBits.set(difficulty,difficulty*2);

        assert challenge.checkZeros(hashBits.toByteArray());
    }

    @Test
    public void testCheckZerosNeg() {
        int difficulty = random.nextInt(100);
        Challenge challenge = hc.generateChallenge(randomString(),difficulty);
        BitSet hashBits = new BitSet(difficulty*2);

        hashBits.set(0,difficulty);

        assert !challenge.checkZeros(hashBits.toByteArray());
    }

    @Test
    public void testHashLength() {
        byte[] seed = new byte[random.nextInt(32)];
        byte[] token = new byte[random.nextInt(32)];

        random.nextBytes(seed);
        random.nextBytes(token);

        assert Challenge.hash(seed,token).length == 20;
    }

    @Test
    public void testWrongMAC() throws Exception {
        byte[] seed = randomString().getBytes("UTF-8");
        Challenge authenticChallenge = new Challenge(seed,1,key1);
        byte[] mac = authenticChallenge.getMAC();
        Challenge nonAuthenticChallenge = new Challenge(seed,2,key1,mac);

        assert !nonAuthenticChallenge.isAuthentic(key1);
    }

    @Test
    public void testWrongSolution() throws Exception {
        Challenge challenge = hc.generateChallenge(randomString(), 50);
        Solution invalidSolution = new Solution(randomString().getBytes("UTF-8"),challenge);

        assert !invalidSolution.isValid(key1);
    }

    @Test
    public void testSolve() throws Exception {
        Challenge challenge = hc.generateChallenge(randomString(),20);

        assert challenge.solve() != null;
    }

    @Test
    public void testCustomSolution() throws Exception {
        Challenge challenge = hc.generateChallenge(randomString(),20);

        assert challenge.solve().isValid(key1);
    }

    @Test
    public void testPurposeCustomSolution() throws Exception {
        Challenge challenge = hc.generateChallenge(HashCash.Purpose.NONE,randomString(),20);
        Solution solution = challenge.solve();

        assert solution.isValid(key1) && solution.getPurpose().equals(HashCash.Purpose.NONE);
    }

    @Test
    public void testAuthSolution() throws Exception {
        Challenge challenge = hc.generateAuthenticationChallenge(randomWorkerID(), randomWorkerID());

        assert challenge.solve().isValid(key1);
    }

    @Test
    public void testRegSolution() throws Exception {
        Challenge challenge = hc.generateRegistrationChallenge(randomWorkerID(), randomWorkerID());

        assert challenge.solve().isValid(key1);
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

    private String randomString() {
        return new BigInteger(130, random).toString(32);
    }

    private WorkerID randomWorkerID() {
        return new WorkerID(keygen.generateKeyPair().getPublic());
    }
}
