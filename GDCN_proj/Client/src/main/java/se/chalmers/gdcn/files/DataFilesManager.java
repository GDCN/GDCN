package se.chalmers.gdcn.files;

import net.tomp2p.peers.PeerAddress;
import net.tomp2p.peers.PeerMapChangeListener;
import se.chalmers.gdcn.control.WorkerReputationManager;
import se.chalmers.gdcn.replica.ReplicaManager;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;

import javax.crypto.SecretKey;
import java.io.*;
import java.security.KeyPair;
import java.util.ArrayList;
import java.util.HashSet;

/**
 * Created by Niklas on 2014-04-01.
 */
public class DataFilesManager {

    private File keyPairLocation;

    private File replicaManagerLocation;

    private File secretKeyLocation;

    private File workerNodeMangerLocation;


    private String filePath;

    private String keyFileName = "keypair";

    private String replicaManagerFileName = "replicaManager";
    private String seretKeyFileName = "secretKey";
    private String workerNodeManagerFileName = "workerNodeManager";

    private String testDirectory;

    private NeighbourFileManager neighbourFileManager;

    public DataFilesManager() {

        this("");

    }

    public DataFilesManager(String dir) {

        this(dir, "");
    }

    //Used when testing
    public DataFilesManager(String dir, String subpart) {

        PathManager.loadDefaultLocation();

        testDirectory = PathManager.getSettingsPath() + dir;

        neighbourFileManager = new NeighbourFileManager(dir, subpart);

        File pathDir = new File(testDirectory);
        pathDir.mkdirs();

        filePath = testDirectory + File.separator + subpart;

        keyPairLocation = new File(filePath + keyFileName);
        replicaManagerLocation = new File(filePath + replicaManagerFileName);
        secretKeyLocation = new File(filePath + seretKeyFileName);
        workerNodeMangerLocation = new File(filePath + workerNodeManagerFileName);


    }

    public void deleteTestDir() {

        File file = new File(testDirectory);

        String[] files = file.list();

        for(String s : files) {
            File f = new File(testDirectory, s);

            f.delete();
        }

        file.delete();
    }



    //KEYPAIR METHODS
    //********************************************\\

    public void saveKeyPair(KeyPair keyPair) {

        ObjectOutputStream oos;

        try {
            FileOutputStream fous = new FileOutputStream(keyPairLocation);
            oos = new ObjectOutputStream(fous);
            try {

                oos.writeObject(keyPair);
            }
            finally {
                oos.close();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public KeyPair getKeypair() {
        KeyPair keypair = null;

        try {
            FileInputStream fis = new FileInputStream(keyPairLocation);

            ObjectInputStream ois = new ObjectInputStream(fis);

            try {
                keypair = (KeyPair) ois.readObject();
            }
            finally {
                ois.close();
            }


        } catch (FileNotFoundException e) {

        } catch (ClassNotFoundException | IOException e) {
            e.printStackTrace();
        }

        return keypair;
    }

    public void removeKeyFile() {
        keyPairLocation.delete();
    }

    //REPLICAMANAGER METHODS
    //********************************************\\

    public void saveReplicaManager(ReplicaManager rm) {

        try {
            FileOutputStream fous = new FileOutputStream(replicaManagerLocation);
            ObjectOutputStream oos = new ObjectOutputStream(fous);

            try {
                oos.writeObject(rm);
            }

            finally {
                oos.close();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public ReplicaManager getReplicaManager() {
        ReplicaManager replicaManager = null;
        try {
            FileInputStream fis = new FileInputStream(replicaManagerLocation);

            ObjectInputStream ois = new ObjectInputStream(fis);

            try {
                replicaManager = (ReplicaManager) ois.readObject();
                replicaManager.resumeTimer();
            }

            finally {
                ois.close();
            }

        } catch (FileNotFoundException e) {

        } catch (ClassNotFoundException | IOException e) {
            e.printStackTrace();
        }

        return replicaManager;
    }

    public void removeReplicaManagerFile() {

        replicaManagerLocation.delete();
    }

    //WORKERNODEMANAGER METHODS
    //********************************************\\

    public void saveWorkerNodeManager(WorkerReputationManager wm) {

        try {
            FileOutputStream fous = new FileOutputStream(workerNodeMangerLocation);
            ObjectOutputStream oos = new ObjectOutputStream(fous);

            try {
                oos.writeObject(wm);
            }
            finally {
                oos.close();
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public WorkerReputationManager getWorkerNodeManager() {
        WorkerReputationManager workerReputationManager = null;

        try {
            FileInputStream fis = new FileInputStream(workerNodeMangerLocation);

            ObjectInputStream ois = new ObjectInputStream(fis);

            try {
                workerReputationManager = (WorkerReputationManager) ois.readObject();
            }

            finally {
                ois.close();
            }


        } catch (FileNotFoundException e) {

        } catch (ClassNotFoundException | IOException e) {
            e.printStackTrace();
        }

        return workerReputationManager;
    }

    public void removeWorkerNodeManagerFile() {
        workerNodeMangerLocation.delete();

    }

    //SECRETKEY METHODS
    //********************************************\\
    public void saveSecretKey(SecretKey secretKey) {


        try {
            FileOutputStream fous = new FileOutputStream(secretKeyLocation);
            ObjectOutputStream oos = new ObjectOutputStream(fous);

            try {
                oos.writeObject(secretKey);
            }

            finally {
                oos.close();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public SecretKey getSecretKey() {
        SecretKey sk = null;

        try {
            FileInputStream fis = new FileInputStream(secretKeyLocation);

            ObjectInputStream ois = new ObjectInputStream(fis);

            try {
                sk = (SecretKey) ois.readObject();
            }

            finally {
                ois.close();
            }


        } catch (FileNotFoundException e) {

        } catch (ClassNotFoundException | IOException e) {
            e.printStackTrace();
        }
        return sk;
    }

    public void removeSecretKeyFile() {
        secretKeyLocation.delete();

    }


    //NEIGHBOURMANAGER METHODS
    //********************************************\\


    public HashSet<PeerAddress> getFileNeighbours() {
        return neighbourFileManager.getFileNeighbours();
    }

    public void removeNeighbourFile() {
        neighbourFileManager.deleteNeighbourFile();
    }

    public PeerMapChangeListener getPeerMapListener() {
        return neighbourFileManager.getPeerMapListener();
    }

    public ArrayList<String[]> getBootstrapNodes() {
        return neighbourFileManager.getBootstrapNodes();
    }



}
