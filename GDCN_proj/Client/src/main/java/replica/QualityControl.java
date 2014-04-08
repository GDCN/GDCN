package replica;

import taskbuilder.Validifier;
import taskbuilder.communicationToClient.ValidityListener;

import java.util.*;
import java.util.concurrent.CountDownLatch;

/**
 * Created by joakim on 4/2/14.
 */
public class QualityControl {

    private final List<Replica> replicas;
    private final String jobName;

    private int bestQuality = 0;
    private final CountDownLatch waitForAll;

    private final Map<Replica, Trust> rewards = new HashMap<>();

    public static Map<Replica, Trust> compareQuality(String jobName, List<Replica> replicas) {
        QualityControl qualityControl = new QualityControl(jobName, replicas);
        return qualityControl.compare();
    }

    private QualityControl(String jobName, List<Replica> replicas) {
        this.jobName = jobName;
        this.replicas = replicas;
        waitForAll = new CountDownLatch(replicas.size());
    }

    private Map<Replica, Trust> compare() {
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

    private synchronized void reward(Replica replica) {
        rewards.put(replica, Trust.TRUSTWORTHY);
        waitForAll.countDown();
    }

    private synchronized void punish(Replica replica) {
        rewards.put(replica, Trust.DECEITFUL);
        waitForAll.countDown();
    }

    private synchronized void unknown(Replica replica) {
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

        private final Validifier validifier;

        private ValidifierRunner(Validifier validifier) {
            this.validifier = validifier;
        }

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
