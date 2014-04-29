package unitTests;

import network.NetworkMessage;
import org.testng.annotations.Test;

import javax.crypto.SealedObject;
import java.math.BigInteger;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.util.Random;

/**
 * Created by Leif on 2014-03-29.
 */
public class MessageTest {
    private KeyPairGenerator keygen;
    private Random random;

    public MessageTest() throws Exception {
        keygen = KeyPairGenerator.getInstance("RSA");
        random = new Random();
    }

    @Test
    public void encryptDecryptMessage() throws Exception {
        KeyPair myKeyPair = keygen.generateKeyPair();
        KeyPair otherKeyPair = keygen.generateKeyPair();
        NetworkMessage message = new NetworkMessage(randomString(), NetworkMessage.Type.REQUEST);

        SealedObject encrypted = message.encrypt(myKeyPair.getPrivate(), otherKeyPair.getPublic());
        NetworkMessage decrypted = NetworkMessage.decrypt(encrypted, otherKeyPair.getPrivate(), myKeyPair.getPublic());

        assert decrypted.equals(message);
    }

    private String randomString() {
        return new BigInteger(130, random).toString(32);
    }
}
