package se.chalmers.gdcn.compare;

import se.chalmers.gdcn.control.ThreadService;
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

    private static final String QUALITY_PROGRAM_NAME = "quality";
    private static final String QUALITY_PROGRAM_SOURCE = "quality.hs";

    private final Set<ByteArray> resultSet;
    private final Map<ByteArray, TrustQuality> trustMap = new HashMap<>();

    private final PathManager pathMan;
    private final String program;
    private final String taskName;
    private final List<String> taskDeps = new ArrayList<>();

    private double bestQuality = -Double.MAX_VALUE;
    private final CountDownLatch waitForAll;

    /**
     * A method for testing the quality and validity of result data, using a job owner defined program
     * @param jobName the name of the job for the task
     * @param taskMeta the task metadata
     * @param resultSet the set of results to test quality
     * @return a map of the result data with their trust level as values
     * @throws IOException
     */
    public static Map<ByteArray, TrustQuality> compareQuality(String jobName, TaskMeta taskMeta, Set<ByteArray> resultSet) throws IOException{
        QualityControl qualityControl = new QualityControl(jobName, taskMeta, resultSet);
        if (qualityControl.program != null) {
            return qualityControl.compare();
        }
        else {
            return qualityControl.fastCompare();
        }
    }

    public static TrustQuality singleQualityTest(String jobName, TaskMeta taskMeta, ByteArray data) throws IOException{
        Set<ByteArray> resultSet = new HashSet<>();
        resultSet.add(data);
        QualityControl qualityControl = new QualityControl(jobName, taskMeta, resultSet);
        if (qualityControl.program != null) {
            return qualityControl.compare().get(data);
        }
        else {
            return qualityControl.fastCompare().get(data);
        }
    }

    private QualityControl(String jobName, TaskMeta taskMeta, Set<ByteArray> resultSet) throws IOException {
        taskName = taskMeta.getTaskName();
        this.resultSet = resultSet;
        waitForAll = new CountDownLatch(resultSet.size());
        pathMan = PathManager.jobOwner(jobName);
        //TODO Follow convention of quality program or quality.hs source
        program = locateQualityProgram(); //new File(pathMan.projectValidDir()).listFiles()[0].getCanonicalPath();
        for (FileDep fileDep : taskMeta.getDependencies()) {
            taskDeps.add(pathMan.projectDir() + fileDep.getFileLocation() + File.separator + fileDep.getFileName());
        }
    }

    private String locateQualityProgram() {
        String qualityProgramPath = pathMan.taskBinaryDir() + QUALITY_PROGRAM_NAME;
        File qualityProgram = new File(qualityProgramPath);
        if (!qualityProgram.canExecute()) {
            boolean status = compileQualityProgram(qualityProgramPath);
            if (!(status && qualityProgram.canExecute())) {
                return null;
            }
        }
        return qualityProgramPath;
    }

    private boolean compileQualityProgram(String qualityProgramPath) {
        String qualitySourcePath = pathMan.taskCodeDir() + QUALITY_PROGRAM_SOURCE;
        File qualitySource = new File(qualitySourcePath);
        if (qualitySource.isFile()) {
            // Compiling quality
            new File(qualityProgramPath).getParentFile().mkdirs();
            String[] command = {"ghc", qualitySourcePath, "-o", qualityProgramPath,
                    "-outputdir", pathMan.projectTempDir()};
            ProcessBuilder processBuilder = new ProcessBuilder(command).inheritIO();

            Process proc = null;
            try {
                proc = processBuilder.start();
                if (proc.waitFor() == 0) {
                    return true;
                }
                else {
                    return false;
                }
            }
            catch (IOException | InterruptedException e) {
                e.printStackTrace();
                return false;
            }
            finally {
                if (proc != null)
                    proc.destroy();
            }
        }
        else {
            System.out.println("Quality source file " + qualitySourcePath + "does not exist");
            return false;
        }
    }

    private Map<ByteArray, TrustQuality> compare() throws IOException {
        int resultID = 0;
        for (ByteArray resultData : resultSet) {
            String resultFile = writeResultFile(resultData.getData(), resultID++);
            Listener listener = new Listener(resultData);
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

    private Map<ByteArray, TrustQuality> fastCompare() {
        for (ByteArray result : resultSet) {
            if (resultSet.size() == 1) {
                // Possible danger if quality program is defined afterward and a new result has quality
                reward(result, -Double.MAX_VALUE);
            }
            else {
                unknown(result, "Undefined quality program");
            }
        }
        return trustMap;
    }

    private String writeResultFile(byte[] data, int resultID) throws IOException {
        FileOutputStream output = null;
        try {
            //TODO check for existing files and don't overwrite
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
        //TODO Remove my file
    }

    private synchronized void punish(ByteArray result) {
        trustMap.put(result, TrustQuality.deceitful());
        waitForAll.countDown();
        // TODO Remove my file
    }

    private synchronized void unknown(ByteArray result, String reason) {
        trustMap.put(result, TrustQuality.unknown(reason));
        waitForAll.countDown();
    }

    private synchronized void addRemaining(String reason) {
        for (ByteArray resultData : resultSet) {
            if (!trustMap.containsKey(resultData)) {
                trustMap.put(resultData, TrustQuality.unknown(reason));
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
}
