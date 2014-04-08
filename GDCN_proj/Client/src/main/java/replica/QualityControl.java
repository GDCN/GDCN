package replica;

import taskbuilder.Validifier;
import taskbuilder.communicationToClient.ValidityListener;
import taskbuilder.fileManagement.PathManager;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.CountDownLatch;

/**
 * Created by joakim on 4/2/14.
 */
public class QualityControl {

    private final List<Replica> replicas;
    private int bestQuality = 0;

    private final PathManager pathMan;
    private final String program;

    private final CountDownLatch waitForAll;
    private final Map<Replica, Trust> rewards = new HashMap<>();

    public static Map<Replica, Trust> compareQuality(String jobName, List<Replica> replicas) {
        QualityControl qualityControl = new QualityControl(jobName, replicas);
        return qualityControl.compare();
    }

    private QualityControl(String jobName, List<Replica> replicas) {
        this.replicas = replicas;
        pathMan = PathManager.jobOwner(jobName);
        waitForAll = new CountDownLatch(replicas.size());
        String program;
        try {
            program = new File(pathMan.projectValidDir()).listFiles()[0].getCanonicalPath();
        }
        catch (IOException e) {
            e.printStackTrace();
            program = null;
        }
        this.program = program;
    }

    private Map<Replica, Trust> compare() {
        for (Replica replica : replicas) {
            try {
                String resultFile = pathMan.projectTempDir() + replica.getReplicaBox().getReplicaID();
                FileOutputStream fos = new FileOutputStream(resultFile);
                fos.write(replica.getResult());
                fos.close();
                Listener listener = new Listener(replica);
                Validifier validifier = new Validifier(listener);
                ValidifierRunner runner = new ValidifierRunner(validifier, resultFile);
                // TODO Limit amount of threads?
                new Thread(runner).start();
            }
            catch (IOException e) {
                e.printStackTrace();
            }
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

        private final String resultFile;
        private final Validifier validifier;

        private ValidifierRunner(Validifier validifier, String resultFile) {
            this.resultFile = resultFile;
            this.validifier = validifier;
        }

        @Override
        public void run() {
            validifier.testResult(program, resultFile);
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
