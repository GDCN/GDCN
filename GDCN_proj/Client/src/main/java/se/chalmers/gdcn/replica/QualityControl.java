package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.files.FileDep;
import se.chalmers.gdcn.files.TaskMeta;
import se.chalmers.gdcn.replica.ReplicaManager.ReplicaID;
import se.chalmers.gdcn.taskbuilder.Validifier;
import se.chalmers.gdcn.taskbuilder.communicationToClient.ValidityListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;
import se.chalmers.gdcn.utils.ByteArray;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.CountDownLatch;

/**
 * Created by joakim on 4/2/14.
 */
public class QualityControl {

    private final Map<ByteArray, Set<ReplicaID>> resultMap;
    private final Map<ByteArray, Trust> trustMap = new HashMap<>();

    private final PathManager pathMan;
    private final String program;
    private final String taskName;
    private final List<String> taskDeps;

    private int bestQuality = Integer.MIN_VALUE;
    private final CountDownLatch waitForAll;

    public static Map<ByteArray, Trust> compareQuality(String jobName, TaskMeta taskMeta, Map<ByteArray, Set<ReplicaID>> resultMap) throws IOException{
        QualityControl qualityControl = new QualityControl(jobName, taskMeta, resultMap);
        return qualityControl.compare();
    }

    private QualityControl(String jobName, TaskMeta taskMeta, Map<ByteArray, Set<ReplicaID>> resultMap) throws IOException {
        this.resultMap = resultMap;
        taskName = taskMeta.getTaskName();
        pathMan = PathManager.jobOwner(jobName);
        waitForAll = new CountDownLatch(resultMap.size());
        program = new File(pathMan.projectValidDir()).listFiles()[0].getCanonicalPath();
        taskDeps = new ArrayList<>();
        for (FileDep fileDep : taskMeta.getDependencies()) {
            taskDeps.add(pathMan.projectDir() + fileDep.getFileLocation() + File.separator + fileDep.getFileName());
        }
    }

    private Map<ByteArray, Trust> compare() throws IOException {
        int resultID = 0;
        for (Map.Entry<ByteArray, Set<ReplicaID>> entry : resultMap.entrySet()) {
            String resultFile = pathMan.projectTempDir() + taskName + "_" + resultID++;
            FileOutputStream fos = new FileOutputStream(resultFile);
            fos.write(entry.getKey().getData());
            fos.close();
            Listener listener = new Listener(entry.getKey());
            Validifier validifier = new Validifier(listener);
            ValidifierRunner runner = new ValidifierRunner(validifier, resultFile);
            // TODO Limit amount of threads?
            new Thread(runner).start();
        }
        try {
            waitForAll.await();
        }
        catch (InterruptedException e) {
            addRemaining();
        }

        return trustMap;
    }

    private synchronized void reward(ByteArray result) {
        trustMap.put(result, Trust.TRUSTWORTHY);
        waitForAll.countDown();
    }

    private synchronized void punish(ByteArray result) {
        trustMap.put(result, Trust.DECEITFUL);
        waitForAll.countDown();
    }

    private synchronized void unknown(ByteArray result) {
        trustMap.put(result, Trust.UNKNOWN);
        waitForAll.countDown();
    }

    private synchronized void punishTrusted(ByteArray result, int newQuality) {
        for (Map.Entry<ByteArray, Trust> entry : trustMap.entrySet()) {
            if (entry.getValue() == Trust.TRUSTWORTHY) {
                entry.setValue(Trust.DECEITFUL);
            }
        }
        trustMap.put(result, Trust.TRUSTWORTHY);
        bestQuality = newQuality;
        waitForAll.countDown();
    }

    private synchronized void addRemaining() {
        for (Map.Entry<ByteArray, Set<ReplicaID>> entry : resultMap.entrySet()) {
            if (!trustMap.containsKey(entry.getKey())) {
                trustMap.put(entry.getKey(), Trust.UNKNOWN);
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
            validifier.testResult(program, resultFile, taskDeps);
        }
    }

    private class Listener implements ValidityListener {

        private final ByteArray myResult;

        private Listener(ByteArray myResult) {
            this.myResult = myResult;
        }

        @Override
        public void validityOk(int quality) {
            if (quality == bestQuality) {
                reward(myResult);
            }
            else if (quality > bestQuality) {
                punishTrusted(myResult, quality);

            }
            else {
                punish(myResult);
            }
        }

        @Override
        public void validityCorrupt() {
            punish(myResult);
        }

        @Override
        public void validityError(String reason) {
            unknown(myResult);
        }
    }
}
