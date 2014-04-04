package replica;

import java.util.Arrays;
import java.util.List;

/**
 * Created by joakim on 4/2/14.
 */
public class EqualityControl {

    public boolean compareData(List<byte[]> data) {
        byte[] prev = null;
        for (byte[] curr : data) {
            if (prev != null && !Arrays.equals(curr, prev)) {
                return false;
            }
            prev = curr;
        }
        return true;
    }
}
