package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.network.WorkerID;

import java.util.HashSet;
import java.util.Set;

/**
 * Created by Leif on 2014-04-15.
 */
public class CanonicalResult {
    private final double quality;
    private final Set<WorkerID> advocatingWorkers = new HashSet<>();
    private final String hash;

    public CanonicalResult() {
        hash = null;
        quality = -1;
    }
    //etc.
    //TODO implement
}
