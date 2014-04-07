package replica;

import taskbuilder.Validifier;
import taskbuilder.communicationToClient.ValidityListener;

import java.util.*;
import java.util.concurrent.CountDownLatch;

/**
 * Created by joakim on 4/2/14.
 */
public class QualityControl {

    private List<Replica> replicas;
    private int bestQuality = 0;
    private CountDownLatch waitForAll = null;

    private final Map<Replica, Trust> rewards = new HashMap<>();

    public QualityControl(List<Replica> replicas) {
        this.replicas = replicas;
    }

    public Map<Replica, Trust> compareQuality() {
        waitForAll = new CountDownLatch(replicas.size());
        for (Replica replica : replicas) {
            Listener listener = new Listener(replica);
            Validifier validifier = new Validifier(listener);
            ValidifierRunner runner = new ValidifierRunner(validifier);
            // TODO Limit amount of threads?
            new Thread(runner).start();
        }
        try {
            waitForAll.await();
        }
        catch (InterruptedException e) {
            addRemaining();
        }
        return rewards;
    }

    public synchronized void reward(Replica replica) {
        rewards.put(replica, Trust.TRUSTWORTHY);
        waitForAll.countDown();
    }

    public synchronized void punish(Replica replica) {
        rewards.put(replica, Trust.DECEITFUL);
        waitForAll.countDown();
    }

    public synchronized void unknown(Replica replica) {
        rewards.put(replica, Trust.UNKNOWN);
        waitForAll.countDown();
    }

    private synchronized void punishTrusted(Replica replica, int newQuality) {
        for (Map.Entry<Replica, Trust> entry : rewards.entrySet()) {
            if (entry.getValue() == Trust.TRUSTWORTHY) {
                entry.setValue(Trust.DECEITFUL);
            }
        }
        rewards.put(replica, Trust.TRUSTWORTHY);
        bestQuality = newQuality;
        waitForAll.countDown();
    }

    private synchronized void addRemaining() {
        for (Replica replica : replicas) {
            if (!rewards.containsKey(replica)) {
                rewards.put(replica, Trust.UNKNOWN);
            }
        }
    }

    private class ValidifierRunner implements Runnable {

        private ValidifierRunner(Validifier validifier) {
            this.validifier = validifier;
        }

        private final Validifier validifier;

        @Override
        public void run() {
            validifier.testResult("TODO Way to get program", "TODO Save data to disk before");
        }
    }

    private class Listener implements ValidityListener {

        private final Replica myReplica;

        private Listener(Replica myReplica) {
            this.myReplica = myReplica;
        }

        @Override
        public void validityOk(int quality) {
            if (quality == bestQuality) {
                reward(myReplica);
            }
            else if (quality > bestQuality) {
                punishTrusted(myReplica, quality);

            }
            else {
                punish(myReplica);
            }
        }

        @Override
        public void validityCorrupt() {
            punish(myReplica);
        }

        @Override
        public void validityError(String reason) {
            unknown(myReplica);
        }
    }
}
