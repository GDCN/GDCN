package unitTests;

import network.Crypto;
import network.NetworkMessage;
import org.testng.annotations.Test;

import javax.crypto.KeyGenerator;
import javax.crypto.SealedObject;
import javax.crypto.SecretKey;
import java.math.BigInteger;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.util.Random;

/**
 * Created by Leif on 2014-03-29.
 */
public class MessageTest {
    private KeyGenerator keygen;
    private Random random;

    public MessageTest() throws Exception {
        keygen = KeyGenerator.getInstance(Crypto.ENCRYPTION_ALGORITHM);
        random = new Random();
    }

    @Test
    public void encryptDecryptMessage() throws Exception {
        SecretKey myKey = keygen.generateKey();
        SecretKey wrongKey = keygen.generateKey();
        NetworkMessage message = new NetworkMessage(randomString(), NetworkMessage.Type.REQUEST);

        SealedObject encrypted = message.encrypt(myKey);
        NetworkMessage decrypted = NetworkMessage.decrypt(encrypted, myKey);

        assert decrypted.equals(message);

        assert NetworkMessage.decrypt(encrypted,wrongKey) != null;
    }

    private String randomString() {
        return new BigInteger(130, random).toString(32);
    }
}
