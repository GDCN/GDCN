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

    private KeyPairGenerator generator;
    private WorkerNodeManager workerNodeManager;

    private KeyPair keyPairA;
    private WorkerID workerA;

    private final static int DEMOTE_REPUTATION = 3;

    @BeforeClass
    public void setupClass() throws NoSuchAlgorithmException {
        generator = KeyPairGenerator.getInstance("RSA");
    }

    @BeforeMethod
    public void setupTest(){
        workerNodeManager = new WorkerNodeManager(WorkerNodeManager.DisciplinaryAction.REMOVE, DEMOTE_REPUTATION);
        keyPairA = generator.generateKeyPair();
        workerA = new WorkerID(keyPairA.getPublic());
    }

    @Test
    public void registerTest() throws NoSuchAlgorithmException {
        assert ! workerNodeManager.isWorkerRegistered(workerA);
        assert workerNodeManager.registerWorker(workerA);

        assert workerNodeManager.isWorkerRegistered(workerA);
        assert workerNodeManager.isWorkerRegistered(new WorkerID(keyPairA.getPublic()));
    }

    @Test
    public void removeTest(){
        assert workerNodeManager.registerWorker(workerA);
        workerNodeManager.reportWorker(workerA, WorkerNodeManager.DisciplinaryAction.REMOVE);

        assert ! workerNodeManager.isWorkerRegistered(workerA);
    }

    @Test
    public void reputationTest(){
        workerNodeManager.registerWorker(workerA);

        assert 0 == workerNodeManager.getReputation(workerA);
        workerNodeManager.promoteWorker(workerA);
        assert 1 == workerNodeManager.getReputation(workerA);

        workerNodeManager.reportWorker(workerA, WorkerNodeManager.DisciplinaryAction.DEMOTE);
        assert 1-DEMOTE_REPUTATION == workerNodeManager.getReputation(workerA);
    }

    @Test
    public void exceptionTestReputation(){
        boolean exceptionThrown = false;
        try{
            workerNodeManager.getReputation(workerA);
        } catch (IllegalArgumentException e){
            exceptionThrown = true;
        }
        assert exceptionThrown;
    }

    @Test
    public void exceptionTestPromote(){
        boolean exceptionThrown = false;
        try{
            workerNodeManager.promoteWorker(workerA);
        } catch (IllegalArgumentException e){
            exceptionThrown = true;
        }
        assert exceptionThrown;
    }

    @Test
    public void exceptionTestReport(){
        boolean exceptionThrown = false;
        try{
            workerNodeManager.reportWorker(workerA);
        } catch (IllegalArgumentException e){
            exceptionThrown = true;
        }
        assert exceptionThrown;
    }
}
