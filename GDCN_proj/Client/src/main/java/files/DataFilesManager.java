package files;

import replica.ReplicaManager;
import taskbuilder.fileManagement.PathManager;

import java.io.*;
import java.nio.file.Path;
import java.security.KeyPair;

/**
 * Created by Niklas on 2014-04-01.
 */
public class DataFilesManager {

    private File keyPairLocation;

    private File replicaManagerLocation;

    private String filepath;

    private String keyFileName;

    private String replicaManagerName;

    public DataFilesManager() {

        this("");

    }

    //Used when testing
    public DataFilesManager(String subpart) {

        PathManager.loadDefaultLocation();

        keyFileName = "keypair";

        replicaManagerName = "replicaManager";

        filepath = PathManager.getSettingsPath() + subpart;

        keyPairLocation = new File(filepath + keyFileName);

        replicaManagerLocation = new File(filepath + replicaManagerLocation);



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

    public void saveWorkerNodeManager(ReplicaManager rm) {

    }

    public ReplicaManager getWorkerNodeManager() {
        return null;
    }

    public void removeWorkerNodeManagerFile() {

    }



}
