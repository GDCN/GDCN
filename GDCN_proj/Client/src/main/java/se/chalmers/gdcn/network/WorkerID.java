package se.chalmers.gdcn.network;

import java.io.Serializable;
import java.security.PublicKey;

/**
 * Created by HalfLeif on 2014-04-01.
 *
 * Class to identify a worker.
*/
public class WorkerID implements Serializable {
    private final PublicKey publicKey;

    public WorkerID(PublicKey publicKey) {
        this.publicKey = publicKey;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof WorkerID)) return false;

        WorkerID workerID = (WorkerID) o;

        if (!publicKey.equals(workerID.publicKey)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return publicKey.hashCode();
    }
}
