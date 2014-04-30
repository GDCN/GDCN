package manualTests;

import net.tomp2p.storage.Data;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Random;
import java.util.TreeSet;

/**
 * Created by Leif on 2014-04-15.
 */
public class SerializerTest {

    /**
     * Relations seem to be preserved after serialization!
     * @throws IOException
     * @throws ClassNotFoundException
     */
    @Test
    public void serial() throws IOException, ClassNotFoundException {
        Ser ser = new Ser();
        ser.list1 = new ArrayList<>();
        ser.list2 = ser.list1;

        assert ser.list2.size() == 0;
        ser.list1.add("Hi");
        assert ser.list2.size() == 1;

        Object obj = new Data(ser).getObject();
        Ser deserialized = (Ser) obj;

        assert deserialized.list1.size() == 1;
        assert deserialized.list2.size() == 1;

        deserialized.list1.add("Some string");
        assert deserialized.list2.size() == 2;
    }

    private static class Ser implements Serializable {
        private ArrayList<String> list1;
        private ArrayList<String> list2;
    }

    @Test
    public void serial2_1() throws IOException, ClassNotFoundException {
        boolean exceptionThrown = false;

        Ser2 ser = new Ser2();
        ser.set  = new TreeSet<>(Ser2.comparator);
        Object obj = null;
        try {
            obj = new Data(ser).getObject();
        } catch (Exception e) {
            exceptionThrown = true;
        }
        assert exceptionThrown;
    }

    @Test
    public void serial2_2() throws IOException, ClassNotFoundException {
        Ser2 ser = new Ser2();
        ser.set  = new TreeSet<>(new Ser2.Comp());
        Object obj = new Data(ser).getObject();
        Ser2 deserialized = (Ser2) obj;
        deserialized.set.add("Hejsan");
        deserialized.set.add("Hej2");
    }

    private static class Ser2 implements Serializable{
        private final static Comparator<String> comparator = new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return 0;
            }
        };
        private static class Comp implements Comparator<String>, Serializable{

            @Override
            public int compare(String o1, String o2) {
                System.out.println("HEJSAN!");
                return 0;
            }
        }

        private TreeSet<String> set;
        private String string = "1";
    }

    @Test
    public void transientTest() throws IOException, ClassNotFoundException {
        TransientTester transientTester = new TransientTester("Hello");
        Object obj = new Data(transientTester).getObject();
        TransientTester deserialized = (TransientTester) obj;

        assert deserialized.random == null;
    }

    private static class TransientTester implements Serializable{
        private final String string;
        private transient Random random = new Random();

        private TransientTester(String string) {
            this.string = string;
            random = new Random();
        }
    }
}
