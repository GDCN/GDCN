package control;

import taskbuilder.fileManagement.PathManager;

import java.io.*;
import java.security.KeyPair;

/**
 * Created by Niklas on 2014-04-01.
 */
public class SettingsManager {

    private PathManager pathManager;

    private File keyPairLocation;

    public SettingsManager() {

        pathManager = PathManager.jobOwner("settings");

        keyPairLocation = new File(pathManager.getSettingsPath() + "keyPair");


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

        } catch (IOException e) {
            e.printStackTrace();
            return null;
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            return null;
        }

    }


}
