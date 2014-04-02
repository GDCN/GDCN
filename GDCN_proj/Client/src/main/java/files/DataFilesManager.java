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

    private String filepath;

    private String keyFileName;

    public DataFilesManager() {

        PathManager.loadDefaultLocation();

        keyFileName = "keypair";

        filepath = PathManager.getSettingsPath();

        keyPairLocation = new File(filepath + keyFileName);

    }

    //Used when testing
    public DataFilesManager(String subpart) {

        PathManager.loadDefaultLocation();

        keyFileName = "keypair";

        filepath = PathManager.getSettingsPath() + subpart;

        keyPairLocation = new File(filepath + keyFileName);

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

    }

    public ReplicaManager getReplicaManager() {
        return null;
    }

    public void removeReplicaManagerFile() {

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
