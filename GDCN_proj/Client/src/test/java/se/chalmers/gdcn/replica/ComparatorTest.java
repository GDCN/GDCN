package se.chalmers.gdcn.replica;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.network.WorkerID;
import utils.TaskHolder;
import utils.WorkerHolder;

import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

/**
 * Created by Leif on 2014-04-17.
 */
public class ComparatorTest {

    private static final float REPUTATION = 3f;
    private static final int REPLICAS = 3;

    private final TaskComparator comparator = new TaskComparator();
    private final TreeSet<TaskCompare> treeSet = new TreeSet<>(comparator);

    private TaskData taskA;
    private TaskData taskB;
    private TaskData cloneA;

    private WorkerID workerA;
    private WorkerID workerB;
    private WorkerID workerC;

    @BeforeClass
    public void setupClass() {
        workerA = WorkerHolder.getWorkerA();
        workerB = WorkerHolder.getWorkerB();
        workerC = WorkerHolder.getWorkerC();
    }

    @BeforeMethod
    private void setup(){
        taskA = create(TaskHolder.getTaskA());
        taskB = create(TaskHolder.getTaskB());
        cloneA = create(TaskHolder.getTaskA());

        treeSet.clear();
    }

    ///////////////////////////////////////////
    // Comparator tests

    @Test
    public void symmetryTest(){
        assert comparator.compare(taskA, taskB) == - comparator.compare(taskB, taskA);
    }

    @Test
    public void equalsTest(){
        assert comparator.compare(taskA, cloneA) == 0;
        assert comparator.compare(cloneA, taskA) == 0;
    }

    @Test
    public void assignLowTest(){
        taskA.giveTask(workerA, 0f);
        assert comparator.compare(taskA, cloneA) > 0;
    }

    @Test
    public void assignHighTest(){
        taskA.giveTask(workerA, REPUTATION);
        assert comparator.compare(taskA, cloneA) < 0;
    }

    ///////////////////////////////////////////
    // TaskData tests

    @Test
    public void returnLowTest(){
        taskA.giveTask(workerA, 0f);
        taskA.giveTask(workerB, 0f);
        taskA.giveTask(workerC, 0f);

        assert ! taskA.enoughGiven();

        assert taskA.value() > 0;
        taskA.returned(workerA);
        taskA.returned(workerB);
        taskA.returned(workerC);

        assert ! taskA.enoughReturned();
    }

    @Test
    public void returnEnoughTest(){
        taskA.giveTask(workerA, REPUTATION);
        assert taskA.value() < 0;

        taskA.giveTask(workerB, 0f);
        taskA.giveTask(workerC, 0f);

        assert taskA.enoughGiven();

        taskA.returned(workerA);
        taskA.returned(workerB);
        taskA.returned(workerC);

        assert taskA.enoughReturned();
    }

    @Test
    public void timeoutTest(){
        float origValue = taskA.value();
        assert origValue > 0;

        taskA.giveTask(workerA, REPUTATION);
        float newValue = taskA.value();
        assert newValue < 0;

        taskA.timedOut(workerA);
        assert taskA.value() == origValue;

        taskA.returned(workerA);
        assert taskA.value() == newValue;
    }

    @Test
    public void orderTest(){
        assert taskA.value() == taskB.value();
        assert ! taskA.order().equals(taskB.order());
        assert taskA.order().equals(cloneA.order());
    }

    ///////////////////////////////////
    // Tree tests

    @Test
    public void testAdd(){
        treeSet.add(taskA);
        treeSet.add(taskB);
        treeSet.add(cloneA);

        assert treeSet.size() == 2;
    }

    @Test
    public void removeTest(){
        treeSet.add(taskA);
        treeSet.add(taskB);

        Set<TaskCompare> otherSet = new HashSet<>();
        otherSet.add(cloneA);

        assert treeSet.removeAll(otherSet);
        assert treeSet.size() == 1;
    }

    @Test
    public void cloneTest(){
        treeSet.add(taskA);
        treeSet.add(taskB);

        Set<TaskCompare> otherSet = (Set<TaskCompare>) treeSet.clone();
        assert otherSet.remove(cloneA);
    }

    private static TaskData create(TaskMeta meta){
        return new TaskData(meta, "job", REPLICAS, REPUTATION);
    }
}
