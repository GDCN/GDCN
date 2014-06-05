package se.chalmers.gdcn.tests;

import se.chalmers.gdcn.control.WorkerReputationManager;
import se.chalmers.gdcn.network.WorkerID;
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

    private WorkerReputationManager workerReputationManager;

    private KeyPair keyPairA;
    private WorkerID workerA;
    private WorkerID myWorkerID;

    private final static int DEMOTE_REPUTATION = 3;

    @BeforeClass
    public void setupClass() throws NoSuchAlgorithmException {
        KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA");
        keyPairA = generator.generateKeyPair();
        workerA = new WorkerID(keyPairA.getPublic());

        myWorkerID = new WorkerID(generator.generateKeyPair().getPublic());
    }

    @BeforeMethod
    public void setupTest(){
        workerReputationManager = new WorkerReputationManager(myWorkerID, DEMOTE_REPUTATION, WorkerReputationManager.DisciplinaryAction.REMOVE);
    }

    @Test
    public void registerTest() throws NoSuchAlgorithmException {
        assert ! workerReputationManager.hasWorkerReputation(workerA);
        assert workerReputationManager.registerWorker(workerA);

        assert ! workerReputationManager.hasWorkerReputation(workerA);
        workerReputationManager.promoteWorker(workerA);

        assert workerReputationManager.hasWorkerReputation(workerA);
        assert workerReputationManager.hasWorkerReputation(new WorkerID(keyPairA.getPublic()));
    }

    @Test
    public void removeTest(){
        assert workerReputationManager.registerWorker(workerA);
        workerReputationManager.reportWorker(workerA, WorkerReputationManager.DisciplinaryAction.REMOVE);

        assert ! workerReputationManager.hasWorkerReputation(workerA);
    }

    @Test
    public void reputationTest(){
        workerReputationManager.registerWorker(workerA);

        assert 0 == workerReputationManager.getReputation(workerA);
        workerReputationManager.promoteWorker(workerA);
        assert 1 == workerReputationManager.getReputation(workerA);

        workerReputationManager.reportWorker(workerA, WorkerReputationManager.DisciplinaryAction.DEMOTE);
        assert 1-DEMOTE_REPUTATION == workerReputationManager.getReputation(workerA);
    }

    @Test
    public void trustHimselfTest(){
        assert workerReputationManager.hasWorkerReputation(myWorkerID);
    }

    @Test
    public void exceptionTestReputation(){
        assert 0 == workerReputationManager.getReputation(workerA);
    }

    @Test
    public void exceptionTestPromote(){
        boolean exceptionThrown = false;
        try{
            workerReputationManager.promoteWorker(workerA);
        } catch (IllegalArgumentException e){
            exceptionThrown = true;
        }
        assert exceptionThrown;
    }

}
