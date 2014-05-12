package se.chalmers.gdcn.tests;

import org.testng.annotations.Test;
import se.chalmers.gdcn.hashcash.WorkerChallengesManager;
import se.chalmers.gdcn.hashcash.Challenge;
import se.chalmers.gdcn.hashcash.Solution;
import se.chalmers.gdcn.network.WorkerID;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import java.security.KeyPairGenerator;
import java.util.Random;

/**
 * Created by weeeeeew on 2014-04-29.
 */
public class WorkerChallengesManagerTest {
    private KeyPairGenerator keygen;
    private WorkerChallengesManager wcm;
    private SecretKey key;
    private final Random random;


    public WorkerChallengesManagerTest() throws Exception {
        keygen = KeyPairGenerator.getInstance("RSA");
        key = KeyGenerator.getInstance("HmacSHA256").generateKey();
        wcm = new WorkerChallengesManager();
        random = new Random();
    }

    @Test
    public void testEverything() throws Exception {
        for(int i = 0; i < 100; i++) {
            WorkerID w1 = randomWorkerID();
            WorkerID w2 = randomWorkerID();

            assert wcm.newWorker(w1);
            assert !wcm.newWorker(w1);

            int s1 = wcm.getCurrentScore(w1);

            int difficulty = random.nextInt(10)+1;
            byte[] seed = new byte[random.nextInt(32)];
            random.nextBytes(seed);

            Challenge challenge = new Challenge(seed,difficulty,key);
            Solution solution = challenge.solve();

            wcm.solvedChallenge(w1,solution);
            wcm.solvedChallenge(w2,solution);

            int s2 = wcm.getCurrentScore(w1);
            int s3 = wcm.getCurrentScore(w2);

            assert s1 != s2 && s1 + difficulty != s2;
            assert s3 != difficulty && s3 != s1;
        }
    }

    private WorkerID randomWorkerID() {
        return new WorkerID(keygen.generateKeyPair().getPublic());
    }
}
