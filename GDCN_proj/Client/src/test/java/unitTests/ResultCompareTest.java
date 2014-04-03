package unitTests;

import org.testng.annotations.Test;
import replica.tester.ResultCompare;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by joakim on 4/3/14.
 */
public class ResultCompareTest {

    @Test
    public void compareEqual() {
        ResultCompare resultCompare = new ResultCompare();
        List<byte[]> list = new ArrayList<>();

        byte[] d1 = {1, 2, 3, 4};
        byte[] d2 = d1;
        byte[] d3 = {1, 2, 3, 0};
        d3[3] = 4;

        list.add(d1);
        list.add(d2);
        list.add(d3);

        assert resultCompare.compareResults(list);
    }

    @Test
    public void compareInequal() {
        ResultCompare resultCompare = new ResultCompare();
        List<byte[]> list = new ArrayList<>();

        byte[] d1 = {1, 2, 3, 4};
        byte[] d2 = {1, 2, 3, 0};

        list.add(d1);
        list.add(d2);

        assert !resultCompare.compareResults(list);
    }
}
