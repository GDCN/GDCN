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

    /**
     * Compares the canonical result with a new result and if they are equal add the new worker as an advocating worker
     * @param data the new worker's data result
     * @param newWorker the new worker ID
     * @return true when the results are equal
     * @throws IOException if the file cannot be read
     */
    public boolean compareNewWorker(ByteArray data, WorkerID newWorker) throws IOException {
        if (hash != data.hashCode()) return false;
        if (Arrays.equals(FileManagementUtils.fromFile(location), data.getData())) {
            advocatingWorkers.add(newWorker);
            return true;
        }
        else {
            return false;
        }
    }

    /**
     *
     * @return quality of the canonical result
     */
    public double getQuality() {
        return quality;
    }

    /**
     *
     * @return the advocating workers of this result
     */
    public Set<WorkerID> getAdvocatingWorkers() {
        return advocatingWorkers;
    }
}
