package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.network.WorkerID;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by Leif on 2014-04-15.
 *
 * Comparable result that doesn't take so much space
 */
public class CanonicalResult {
    private final double quality;
    private final String hash;
    private final File location;
    private final Set<WorkerID> advocatingWorkers = new HashSet<>();

    public CanonicalResult() {
        hash = null;
        quality = -1;
        location = null;
    }
    //etc.
    //TODO implement
}
