package se.chalmers.gdcn.files;

import se.chalmers.gdcn.control.WorkerNodeManager;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.peers.PeerMapChangeListener;
import se.chalmers.gdcn.replica.ReplicaManager;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;

import javax.crypto.SecretKey;
import java.io.*;
import java.security.KeyPair;
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

        testDirectory = PathManager.getSettingsPath() + File.separator + dir;

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

        System.out.println("Directory was deleted: " + file.delete());
    }



    //KEYPAIR METHODS
    //********************************************\\

    public void saveKeyPair(KeyPair keyPair) {

        try {
            FileOutputStream fous = new FileOutputStream(keyPairLocation);
            ObjectOutputStream oos = new ObjectOutputStream(fous);

            oos.writeObject(keyPair);

            oos.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public KeyPair getKeypair() {

        try {
            FileInputStream fis = new FileInputStream(keyPairLocation);

            ObjectInputStream ois = new ObjectInputStream(fis);

            KeyPair keypair = (KeyPair) ois.readObject();

            ois.close();

            return keypair;

        } catch (FileNotFoundException e) {
            return null;
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            return null;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
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

            //OBS Must use the clone of ReplicaManager because of how ReplicaTimer is implemented
            oos.writeObject(rm.clone());

            oos.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public ReplicaManager getReplicaManager() {
        try {
            FileInputStream fis = new FileInputStream(replicaManagerLocation);

            ObjectInputStream ois = new ObjectInputStream(fis);

            ReplicaManager replicaManager = (ReplicaManager) ois.readObject();

            ois.close();

            replicaManager.resumeTimer();
            return replicaManager;

        } catch (FileNotFoundException e) {
            return null;
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            return null;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        } finally {
            ;
            //TODO close stream here!
        }
    }

    public void removeReplicaManagerFile() {

        replicaManagerLocation.delete();
    }

    //WORKERNODEMANAGER METHODS
    //********************************************\\

    public void saveWorkerNodeManager(WorkerNodeManager wm) {

        try {
            FileOutputStream fous = new FileOutputStream(workerNodeMangerLocation);
            ObjectOutputStream oos = new ObjectOutputStream(fous);

            oos.writeObject(wm);

            oos.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public WorkerNodeManager getWorkerNodeManager() {
        try {
            FileInputStream fis = new FileInputStream(workerNodeMangerLocation);

            ObjectInputStream ois = new ObjectInputStream(fis);

            WorkerNodeManager workerNodeManager = (WorkerNodeManager) ois.readObject();

            ois.close();

            return workerNodeManager;

        } catch (FileNotFoundException e) {
            return null;
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            return null;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
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

            oos.writeObject(secretKey);

            oos.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public SecretKey getSecretKey() {
        try {
            FileInputStream fis = new FileInputStream(secretKeyLocation);

            ObjectInputStream ois = new ObjectInputStream(fis);

            SecretKey sk = (SecretKey) ois.readObject();

            ois.close();

            return sk;

        } catch (FileNotFoundException e) {
            return null;
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            return null;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }

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



}
