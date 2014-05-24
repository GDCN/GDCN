package se.chalmers.gdcn.tests;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import se.chalmers.gdcn.demo.WorkerNames;
import se.chalmers.gdcn.network.WorkerID;
import utils.WorkerHolder;

/**
 * Created by Leif on 2014-05-24.
 */
public class WorkerNameTest {

    private WorkerNames names;
    private final WorkerID workerA = WorkerHolder.getWorkerA();
    private final WorkerID workerB = WorkerHolder.getWorkerB();

    private static final String myselfString = "Myself";

    @BeforeMethod
    public void before(){
        names = new WorkerNames();
    }

    @Test
    public void registerWorkerTest(){
        assert names.registerName(workerA);
        assert ! names.registerName(workerA);
        assert names.getName(workerA) != null;
        assert names.getName(workerB) == null;
    }

    @Test
    public void registerSelfTest(){
        names.setLocalID(workerA);
        assert names.getName(workerA).equals(myselfString);
        assert ! names.registerName(workerA);
    }

    @Test
    public void reRegisterSelfTest(){
        names.setLocalID(workerA);
        names.setLocalID(workerB);

        String a = names.getName(workerA);
        assert a != null;
        assert !a.equals(myselfString);
        
        assert names.getName(workerB).equals(myselfString);
    }
}
