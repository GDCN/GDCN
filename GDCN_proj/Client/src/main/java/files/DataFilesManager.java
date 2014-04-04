package files;

import control.WorkerNodeManager;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.peers.PeerMapChangeListener;
import replica.ReplicaManager;
import taskbuilder.fileManagement.PathManager;

import java.io.*;
import java.security.KeyPair;
import java.util.HashSet;

/**
 * Created by Niklas on 2014-04-01.
 */
public class DataFilesManager {

    private File keyPairLocation;

    private File replicaManagerLocation;

    private String filePath;

    private String keyFileName;

    private String replicaManagerFileName;

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

        keyFileName = "keypair";

        replicaManagerFileName = "replicaManager";

        File pathDir = new File(testDirectory);

        pathDir.mkdirs();

        filePath = testDirectory + File.separator + subpart;

        keyPairLocation = new File(filePath + keyFileName);

        replicaManagerLocation = new File(filePath + replicaManagerFileName);


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

            oos.writeObject(rm);

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

            return replicaManager;

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

    public void removeReplicaManagerFile() {

        replicaManagerLocation.delete();
    }

    //WORKERNODEMANAGER METHODS
    //********************************************\\

    public void saveWorkerNodeManager(WorkerNodeManager workerNodeManager) {

    }

    public WorkerNodeManager getWorkerNodeManager() {
        return null;
    }

    public void removeWorkerNodeManagerFile() {

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
