package unitTests;

import org.testng.annotations.Test;
import se.chalmers.gdcn.utils.Identifier;

/**
 * Created by HalfLeif on 2014-04-16.
 *
 * Redundant test perhaps...
 */
public class IdentifierTest {

    private static class Id extends Identifier{
        public Id(String id) {
            super(id);
        }
    }

    private final String key = "Some key";

    @Test
    public void equalsTest(){
        Id id1 = new Id(key);
        Id id2 = new Id(key);

        assert id1.equals(id2);
        assert id2.equals(id1);
    }
}
