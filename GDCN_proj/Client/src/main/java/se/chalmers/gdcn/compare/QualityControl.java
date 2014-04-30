package se.chalmers.gdcn.compare;

import se.chalmers.gdcn.control.ThreadService;
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

    private Map<ByteArray, Set<ReplicaID>> resultMap;
    private Map<ByteArray, TrustQuality> trustMap;
    private ByteArray singleResult;
    private TrustQuality singleTrust;

    private final PathManager pathMan;
    private final String program;
    private final String taskName;
    private final List<String> taskDeps;

    private double bestQuality = Double.MIN_VALUE;
    private CountDownLatch waitForAll;

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

    public static TrustQuality singleQualityTest(String jobName, TaskMeta taskMeta, ByteArray data) throws IOException{
        QualityControl qualityControl = new QualityControl(jobName, taskMeta, data);
        return qualityControl.quality();
    }

    private QualityControl(String jobName, TaskMeta taskMeta, Map<ByteArray, Set<ReplicaID>> resultMap) throws IOException {
        this(jobName, taskMeta);
        trustMap = new HashMap<>();
        this.resultMap = resultMap;
        waitForAll = new CountDownLatch(resultMap.size());
    }

    private QualityControl(String jobName, TaskMeta taskMeta, ByteArray data) throws IOException {
        this(jobName, taskMeta);
        singleResult = data;
    }

    private QualityControl(String jobName, TaskMeta taskMeta) throws IOException {
        taskName = taskMeta.getTaskName();
        pathMan = PathManager.jobOwner(jobName);
        program = new File(pathMan.projectValidDir()).listFiles()[0].getCanonicalPath();
        taskDeps = new ArrayList<>();
        for (FileDep fileDep : taskMeta.getDependencies()) {
            taskDeps.add(pathMan.projectDir() + fileDep.getFileLocation() + File.separator + fileDep.getFileName());
        }
    }

    private Map<ByteArray, TrustQuality> compare() throws IOException {
        int resultID = 0;
        for (Map.Entry<ByteArray, Set<ReplicaID>> entry : resultMap.entrySet()) {
            String resultFile = writeResultFile(entry.getKey().getData(), resultID++);
            Listener listener = new Listener(entry.getKey());
            Validifier validifier = new Validifier(listener);
            ValidifierRunner runner = new ValidifierRunner(validifier, resultFile);

            ThreadService.submit(runner);
            //new Thread(runner).start();
        }
        try {
            waitForAll.await();
        }
        catch (InterruptedException e) {
            addRemaining(e.getMessage());
        }

        return trustMap;
    }

    private TrustQuality quality() throws IOException {
        String resultFile = writeResultFile(singleResult.getData(), Math.abs(singleResult.hashCode()));
        ListenerSingle listener = new ListenerSingle();
        Validifier validifier = new Validifier(listener);
        validifier.testResult(program, resultFile, taskDeps);
        return singleTrust;
    }

    private String writeResultFile(byte[] data, int resultID) throws IOException {
        FileOutputStream output = null;
        try {
            String resultFile = pathMan.projectTempDir() + taskName + "_" + resultID;
            File parent = new File(resultFile).getParentFile();
            parent.mkdirs();
            output = new FileOutputStream(resultFile);
            output.write(data);
            return resultFile;
        }
        finally {
            if (output != null)
                output.close();
        }
    }

    private synchronized void reward(ByteArray result, double quality) {
        if (quality == bestQuality) {
            trustMap.put(result, TrustQuality.trustworthy(quality));
        }
        else if (quality > bestQuality) {
            for (Map.Entry<ByteArray, TrustQuality> entry : trustMap.entrySet()) {
                if (entry.getValue().getTrust() == Trust.TRUSTWORTHY) {
                    entry.setValue(TrustQuality.deceitful());
                }
            }
            trustMap.put(result, TrustQuality.trustworthy(quality));
            bestQuality = quality;
        }
        else {
            trustMap.put(result, TrustQuality.deceitful());
        }
        waitForAll.countDown();
    }

    private synchronized void punish(ByteArray result) {
        trustMap.put(result, TrustQuality.deceitful());
        waitForAll.countDown();
    }

    private synchronized void unknown(ByteArray result, String reason) {
        trustMap.put(result, TrustQuality.unknown(reason));
        waitForAll.countDown();
    }

    private synchronized void addRemaining(String reason) {
        for (Map.Entry<ByteArray, Set<ReplicaID>> entry : resultMap.entrySet()) {
            if (!trustMap.containsKey(entry.getKey())) {
                trustMap.put(entry.getKey(), TrustQuality.unknown(reason));
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
            unknown(myResult, reason);
        }
    }

    private class ListenerSingle implements ValidityListener {

        @Override
        public void validityOk(double quality) {
            singleTrust = TrustQuality.trustworthy(quality);
        }

        @Override
        public void validityCorrupt() {
            singleTrust = TrustQuality.deceitful();
        }

        @Override
        public void validityError(String reason) {
            singleTrust = TrustQuality.unknown(reason);
        }
    }
}
