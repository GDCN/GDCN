package manualTests;

import net.tomp2p.storage.Data;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;

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
}
