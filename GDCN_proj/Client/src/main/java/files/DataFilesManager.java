package files;

import taskbuilder.fileManagement.PathManager;

import java.io.*;
import java.security.KeyPair;

/**
 * Created by Niklas on 2014-04-01.
 */
public class DataFilesManager {

    private File keyPairLocation;

    private String filepath;

    private String keyFileName;

    public DataFilesManager() {

        keyFileName = "keypair";

        filepath = PathManager.getSettingsPath();

        keyPairLocation = new File(filepath + keyFileName);

    }

    public DataFilesManager(String subpart) {

        keyFileName = "keypair";

        filepath = PathManager.getSettingsPath() + subpart;

        keyPairLocation = new File(filepath + keyFileName);


    }

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


}
