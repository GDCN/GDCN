package se.chalmers.gdcn.replica;

import org.testng.annotations.Test;
import se.chalmers.gdcn.compare.EqualityControl;
import se.chalmers.gdcn.replica.ReplicaManager.ReplicaID;
import se.chalmers.gdcn.utils.ByteArray;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Created by joakim on 4/3/14.
 */
public class EqualityControlTest {

    private ReplicaID replicaA = new ReplicaID("A");
    private ReplicaID replicaB = new ReplicaID("B");
    private ReplicaID replicaC = new ReplicaID("C");
    private ReplicaID replicaD = new ReplicaID("D");

    private byte[] firstResult = {1, 2, 3, 4};
    private byte[] equalResult = {1, 2, 3, 4};
    private byte[] nonEqualResult = {1, 2, 3, 0};

    @Test
    public void compareEqual() {
        Map<ReplicaID, byte[]> results = new HashMap<>();
        results.put(replicaA, firstResult);
        results.put(replicaB, firstResult);
        results.put(replicaC, equalResult);

        Map<ByteArray,Set<ReplicaID>> compareData = EqualityControl.compareData(results);
        assert compareData.size() == 1;

        Set<ReplicaID> replicaIDs = compareData.get(new ByteArray(firstResult));
        assert replicaIDs != null;
        assert replicaIDs.contains(replicaA);
        assert replicaIDs.contains(replicaB);
        assert replicaIDs.contains(replicaC);
    }

    @Test
    public void compareInequal() {
        Map<ReplicaID, byte[]> results = new HashMap<>();
        results.put(replicaA, firstResult);
        results.put(replicaD, nonEqualResult);

        Map<ByteArray, Set<ReplicaID>> compareData = EqualityControl.compareData(results);
        assert compareData.size() == 2;

        Set<ReplicaID> firsts = compareData.get(new ByteArray(firstResult));
        assert firsts != null;
        assert firsts.contains(replicaA);

        Set<ReplicaID> nonEquals = compareData.get(new ByteArray(nonEqualResult));
        assert nonEquals != null;
        assert nonEquals.contains(replicaD);
    }

    @Test
    public void compareMixed() {
        Map<ReplicaID, byte[]> results = new HashMap<>();
        results.put(replicaA, firstResult);
        results.put(replicaC, equalResult);
        results.put(replicaD, nonEqualResult);

        Map<ByteArray, Set<ReplicaID>> compareData = EqualityControl.compareData(results);
        assert compareData.size() == 2;

        Set<ReplicaID> firsts = compareData.get(new ByteArray(firstResult));
        assert firsts != null;
        assert firsts.contains(replicaA);
        assert firsts.contains(replicaC);

        Set<ReplicaID> nonEquals = compareData.get(new ByteArray(nonEqualResult));
        assert nonEquals != null;
        assert nonEquals.contains(replicaD);
    }
}
