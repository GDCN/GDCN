import control.WorkerNodeManager;
import network.WorkerID;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

/**
 * Created by Leif on 2014-04-01.
 */
public class WorkerManagementTest {

    private KeyPairGenerator generator = null;
    private WorkerNodeManager workerNodeManager;

    @BeforeClass
    public void setupClass() throws NoSuchAlgorithmException {
        generator = KeyPairGenerator.getInstance("RSA");
    }

    @BeforeMethod
    public void setupTest(){
        workerNodeManager = new WorkerNodeManager();
    }

    @Test
    public void registerTest() throws NoSuchAlgorithmException {
        KeyPair keyPairA = generator.generateKeyPair();
        WorkerID workerA = new WorkerID(keyPairA.getPublic());

        assert ! workerNodeManager.isWorkerRegistered(workerA);
        assert workerNodeManager.registerWorker(workerA);

        assert workerNodeManager.isWorkerRegistered(workerA);
        assert workerNodeManager.isWorkerRegistered(new WorkerID(keyPairA.getPublic()));
    }

    @Test
    public void removeTest(){
        KeyPair keyPairA = generator.generateKeyPair();
        WorkerID workerA = new WorkerID(keyPairA.getPublic());

        assert workerNodeManager.registerWorker(workerA);
        workerNodeManager.reportWorker(workerA, WorkerNodeManager.DisciplinaryAction.REMOVE);

        assert ! workerNodeManager.isWorkerRegistered(workerA);
    }
}
