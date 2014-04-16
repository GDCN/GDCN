package se.chalmers.gdcn.replica;

import se.chalmers.gdcn.files.FileDep;
import se.chalmers.gdcn.files.TaskMeta;
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

    private final Set<ByteArray> resultSet;
    private final Map<ByteArray, Trust> trustMap = new HashMap<>();

    private final PathManager pathMan;
    private final String program;
    private final String taskName;
    private final List<String> taskDeps;

    private int bestQuality = Integer.MIN_VALUE;
    private final CountDownLatch waitForAll;

    /**
     * A method for testing the quality and validity of result data, using a job owner defined program
     * @param jobName the name of the job for the task
     * @param taskMeta the task metadata
     * @param resultSet the set of results to test quality
     * @return a map of the result data with their trust level as values
     * @throws IOException
     */
    public static Map<ByteArray, Trust> compareQuality(String jobName, TaskMeta taskMeta, Set<ByteArray> resultSet) throws IOException{
        QualityControl qualityControl = new QualityControl(jobName, taskMeta, resultSet);
        return qualityControl.compare();
    }

    private QualityControl(String jobName, TaskMeta taskMeta, Set<ByteArray> resultSet) throws IOException {
        this.resultSet = resultSet;
        taskName = taskMeta.getTaskName();
        pathMan = PathManager.jobOwner(jobName);
        waitForAll = new CountDownLatch(resultSet.size());
        program = new File(pathMan.projectValidDir()).listFiles()[0].getCanonicalPath();
        taskDeps = new ArrayList<>();
        for (FileDep fileDep : taskMeta.getDependencies()) {
            taskDeps.add(pathMan.projectDir() + fileDep.getFileLocation() + File.separator + fileDep.getFileName());
        }
    }

    private Map<ByteArray, Trust> compare() throws IOException {
        int resultID = 0;
        for (ByteArray data : resultSet) {
            String resultFile = pathMan.projectTempDir() + taskName + "_" + resultID++;
            FileOutputStream fos = new FileOutputStream(resultFile);
            fos.write(data.getData());
            fos.close();
            Listener listener = new Listener(data);
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

    private synchronized void reward(ByteArray result, int quality) {
        if (quality == bestQuality) {
            trustMap.put(result, Trust.TRUSTWORTHY);
        }
        else if (quality > bestQuality) {
            for (Map.Entry<ByteArray, Trust> entry : trustMap.entrySet()) {
                if (entry.getValue() == Trust.TRUSTWORTHY) {
                    entry.setValue(Trust.DECEITFUL);
                }
            }
            trustMap.put(result, Trust.TRUSTWORTHY);
            bestQuality = quality;
        }
        else {
            trustMap.put(result, Trust.DECEITFUL);
        }
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

    private synchronized void addRemaining() {
        for (ByteArray data : resultSet) {
            if (!trustMap.containsKey(data)) {
                trustMap.put(data, Trust.UNKNOWN);
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
