package unitTests;

import org.testng.annotations.Test;
import replica.EqualityControl;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by joakim on 4/3/14.
 */
public class EqualityControlTest {

    @Test
    public void compareEqual() {
        EqualityControl resultCompare = new EqualityControl();
        List<byte[]> list = new ArrayList<>();

        byte[] d1 = {1, 2, 3, 4};
        byte[] d2 = d1;
        byte[] d3 = {1, 2, 3, 0};
        d3[3] = 4;

        list.add(d1);
        list.add(d2);
        list.add(d3);

        assert resultCompare.compareData(list);
    }

    @Test
    public void compareInequal() {
        EqualityControl resultCompare = new EqualityControl();
        List<byte[]> list = new ArrayList<>();

        byte[] d1 = {1, 2, 3, 4};
        byte[] d2 = {1, 2, 3, 0};

        list.add(d1);
        list.add(d2);

        assert !resultCompare.compareData(list);
    }
}
