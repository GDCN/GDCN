package replica;

import network.WorkerID;
import utils.ByteArray;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by joakim on 4/2/14.
 */
public class EqualityControl {

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