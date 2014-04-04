package replica;

import taskbuilder.Validifier;
import taskbuilder.communicationToClient.ValidityListener;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Created by joakim on 4/2/14.
 */
public class ResultCompare {

    public Map<Replica, Object> compareQuality(List<Replica> replicas) {
        int bestQuality = 0;
        for (Replica replica : replicas) {
            Listener listener = new Listener(replica);
            Validifier validifier = new Validifier(listener);
            validifier.testResult("TODO", "TODO");
        }
        return null;
    }

    public boolean compareResults(List<Replica> replicas) {
        byte[] prev = null;
        for (Replica replica : replicas) {
            byte[] curr = replica.getResult();
            if (prev != null && !Arrays.equals(curr, prev)) {
                return false;
            }
            prev = curr;
        }
        return true;
    }

    private class Listener implements ValidityListener {

        private final Replica myReplica;

        private Listener(Replica myReplica) {
            this.myReplica = myReplica;
        }

        @Override
        public void validityOk(int quality) {

        }

        @Override
        public void validityCorrupt() {

        }

        @Override
        public void validityError(String reason) {

        }
    }
}
