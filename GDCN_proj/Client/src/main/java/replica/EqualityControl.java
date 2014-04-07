package replica;

import java.util.List;

/**
 * Created by joakim on 4/2/14.
 */
public class EqualityControl {

    public static boolean compareData(List<byte[]> data) {
        byte[] prev = null;
        for (byte[] curr : data) {
            if (prev != null && curr.length != prev.length) {
                return false;
            }
            prev = curr;
        }

        prev = null;
        for (byte[] curr : data) {
            if (prev != null) {
                for (int i = 0; i < curr.length; i++) {
                    if (curr[i] != prev[i]) {
                        return false;
                    }
                }
            }
            prev = curr;
        }
        return true;
    }
}
