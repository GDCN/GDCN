package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.network.WorkerID;
import se.chalmers.gdcn.utils.ByteArray;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by joakim on 4/2/14.
 */
public class EqualityControl {

    public static Map<ByteArray, List<WorkerID>> compareData(List<ReplicaOLD> replicas) {
        Map<ByteArray, List<WorkerID>> map = new HashMap<>();

        for (ReplicaOLD replica : replicas) {
            ByteArray result = new ByteArray(replica.getResult());
            //TODO fix all of this, result is stored in a different way after refactor
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