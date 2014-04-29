package se.chalmers.gdcn.compare;

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
 * Class for test quality of results
 */
public class QualityControl {

    private final Map<ByteArray, Set<ReplicaID>> resultMap;
    private final Map<ByteArray, TrustQuality> trustMap = new HashMap<>();

    private final PathManager pathMan;
    private final String program;
    private final String taskName;
    private final List<String> taskDeps;

    private double bestQuality = Double.MIN_VALUE;
    private final CountDownLatch waitForAll;

    /**
     * A method for testing the quality and validity of result data, using a job owner defined program
     * @param jobName the name of the job for the task
     * @param taskMeta the task metadata
     * @param resultMap the map of results to test quality
     * @return a map of the result data with their trust level as values
     * @throws IOException
     */
    public static Map<ByteArray, TrustQuality> compareQuality(String jobName, TaskMeta taskMeta, Map<ByteArray, Set<ReplicaID>> resultMap) throws IOException{
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

    private Map<ByteArray, TrustQuality> compare() throws IOException {
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

    private synchronized void reward(ByteArray result, double quality) {
        if (quality == bestQuality) {
            trustMap.put(result, new TrustQuality(Trust.TRUSTWORTHY, quality));
        }
        else if (quality > bestQuality) {
            for (Map.Entry<ByteArray, TrustQuality> entry : trustMap.entrySet()) {
                if (entry.getValue().getTrust() == Trust.TRUSTWORTHY) {
                    entry.getValue().setTrust(Trust.DECEITFUL);
                }
            }
            trustMap.put(result, new TrustQuality(Trust.TRUSTWORTHY, quality));
            bestQuality = quality;
        }
        else {
            trustMap.put(result, new TrustQuality(Trust.DECEITFUL));
        }
        waitForAll.countDown();
    }

    private synchronized void punish(ByteArray result) {
        trustMap.put(result, new TrustQuality(Trust.DECEITFUL));
        waitForAll.countDown();
    }

    private synchronized void unknown(ByteArray result) {
        trustMap.put(result, new TrustQuality(Trust.UNKNOWN));
        waitForAll.countDown();
    }

    private synchronized void addRemaining() {
        for (Map.Entry<ByteArray, Set<ReplicaID>> entry : resultMap.entrySet()) {
            if (!trustMap.containsKey(entry.getKey())) {
                trustMap.put(entry.getKey(), new TrustQuality(Trust.UNKNOWN));
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
        public void validityOk(double quality) {
            reward(myResult, quality);
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
