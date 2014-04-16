package manualTests;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import se.chalmers.gdcn.replica.TaskCompare;

import java.io.Serializable;
import java.util.Comparator;
import java.util.TreeSet;

/**
 * Created by HalfLeif on 2014-04-16.
 */
public class TreeTest {
    TreeSet<TaskCompare> treeSet = new TreeSet<>(new TaskComparator());

    TaskDummy taskA = new TaskDummy(1);
    TaskDummy taskB = new TaskDummy(2);
    TaskDummy taskC = new TaskDummy(1);

    @BeforeMethod
    public void setup(){
        treeSet.clear();
    }

    @Test
    public void testAdd(){
        treeSet.add(taskA);
        treeSet.add(taskB);
        treeSet.add(taskC);

        assert treeSet.size() == 2;
    }

    private static class TaskDummy implements TaskCompare{

        private final int id;

        private TaskDummy(int id) {
            this.id = id;
        }

        @Override
        public float value() {
            return 0;
        }

        @Override
        public String order() {
            return ""+id;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof TaskDummy)) return false;

            TaskDummy taskDummy = (TaskDummy) o;

            if (id != taskDummy.id) return false;

            return true;
        }

        @Override
        public int hashCode() {
            return id;
        }

        @Override
        public String toString() {
            return "TaskDummy{" +
                    "id=" + id +
                    '}';
        }
    }

    private static class TaskComparator implements Comparator<TaskCompare>, Serializable {
        @Override
        public int compare(TaskCompare o1, TaskCompare o2) {
            if(o1.value()>o2.value()){
                return 1;
            } else if(o1.value()<o2.value()){
                return -1;
            } else{
                return o1.order().compareTo(o2.order());
            }
        }
    }
}
