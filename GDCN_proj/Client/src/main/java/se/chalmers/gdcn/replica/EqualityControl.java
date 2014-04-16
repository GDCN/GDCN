package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.utils.ByteArray;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Class for testing equality of results
 */
public class EqualityControl {

    /**
     * A method for testing equality of replica result data
     * @param replicas the replicas to test
     * @return a map of data with a list of the workers of this result
     */
    public static Map<ByteArray, List<WorkerID>> compareData(List<Replica> replicas) {
        Map<ByteArray, List<WorkerID>> map = new HashMap<>();

        for (Replica replica : replicas) {
            ByteArray result = new ByteArray(replica.getResult());
            List<WorkerID> list = map.get(result);
            if (list == null) {
                list = new ArrayList<>();
                map.put(result, list);
            }
            list.add(replica.getWorker());
        }

        return map;
    }
}