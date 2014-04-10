package unitTests;

import network.Crypto;
import network.NetworkMessage;
import org.testng.annotations.Test;

import javax.crypto.SealedObject;
import java.io.IOException;
import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.SignatureException;
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

        SealedObject encrypted = message.signAndEncrypt(myKeyPair.getPrivate(), otherKeyPair.getPublic());
        NetworkMessage decrypted = NetworkMessage.decryptAndVerify(encrypted, otherKeyPair.getPrivate(), myKeyPair.getPublic());

        assert decrypted.getType() == message.getType();
        assert decrypted.getObject().equals(message.getObject());
    }

    private String randomString() {
        return new BigInteger(130, random).toString(32);
    }
}
