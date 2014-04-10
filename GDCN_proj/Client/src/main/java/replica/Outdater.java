package replica;

import java.io.Serializable;

/**
 * Created by HalfLeif on 2014-04-08.
 */
public interface Outdater extends Serializable{
    void replicaOutdated(String replicaID);
}
