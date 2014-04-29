package se.chalmers.gdcn.replica;

import edu.emory.mathcs.backport.java.util.Arrays;
import se.chalmers.gdcn.files.FileManagementUtils;
import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.utils.ByteArray;

import java.io.*;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by Leif on 2014-04-15.
 *
 * Comparable result that doesn't take so much space
 */
public class CanonicalResult {
    private final double quality;
    private final int hash; //TODO Use better hash algorithm?
    private final File location;
    private final Set<WorkerID> advocatingWorkers = new HashSet<>();

    CanonicalResult(ByteArray data, double quality, Set<WorkerID> advocatingWorkers, File location) {
        this.hash = data.hashCode();
        this.quality = quality;
        this.location = location;
        this.advocatingWorkers.addAll(advocatingWorkers);
        FileManagementUtils.toFile(location, data.getData());
    }

    public boolean compareNewWorker(ByteArray data, Set<WorkerID> newWorkers) throws IOException {
        if (hash != data.hashCode()) return false;
        if (Arrays.equals(FileManagementUtils.fromFile(location), data.getData())) {
            advocatingWorkers.addAll(newWorkers);
            return true;
        }
        else {
            return false;
        }
    }

    public double getQuality() {
        return quality;
    }

    public Set<WorkerID> getAdvocatingWorkers() {
        return advocatingWorkers;
    }
}
