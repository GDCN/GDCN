package se.chalmers.gdcn.compare;

import se.chalmers.gdcn.replica.ReplicaManager.ReplicaID;
import se.chalmers.gdcn.utils.ByteArray;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Created by joakim on 4/2/14.
 */
public class EqualityControl {

    public static Map<ByteArray, Set<ReplicaID>> compareData(Map<ReplicaID, byte[]> resultMap) {
        Map<ByteArray, Set<ReplicaID>> returnMap = new HashMap<>();
        for (ReplicaID replicaID : resultMap.keySet()) {
            ByteArray result = new ByteArray(resultMap.get(replicaID));

            Set<ReplicaID> set = returnMap.get(result);
            if (set == null) {
                set = new HashSet<>();
                returnMap.put(result, set);
            }
            set.add(replicaID);
        }
        return returnMap;
    }
}