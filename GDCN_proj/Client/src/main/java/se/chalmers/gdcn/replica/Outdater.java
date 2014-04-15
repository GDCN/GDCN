package se.chalmers.gdcn.replica;

import java.io.Serializable;

/**
 * Created by HalfLeif on 2014-04-08.
 * @deprecated
 */
public interface Outdater extends Serializable{
    void replicaOutdated(String replicaID);
}
