package unitTests;

import network.Crypto;
import network.NetworkMessage;
import org.testng.annotations.Test;

import javax.crypto.IllegalBlockSizeException;
import javax.crypto.KeyGenerator;
import javax.crypto.SealedObject;
import javax.crypto.SecretKey;
import java.io.IOException;
import java.io.Serializable;
import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.InvalidParameterException;
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
        keygen = KeyGenerator.getInstance(Crypto.SECRET_KEY_ALGORITHM);
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

        assert NetworkMessage.decrypt(encrypted,wrongKey) == null;
    }

    @Test(expectedExceptions = InvalidParameterException.class)
    public void testDecryptOther() throws InvalidKeyException, IOException, IllegalBlockSizeException {
        Serializable s = new Integer(1);
        SecretKey key = keygen.generateKey();

        SealedObject encrypted = Crypto.encrypt(s,key);

        NetworkMessage.decrypt(encrypted,key);
    }

    @Test
    public void testEquality() throws Exception {
        String msg1 = randomString();
        String msg2 = new String(msg1);

        NetworkMessage nm1 = new NetworkMessage(msg1, NetworkMessage.Type.REQUEST);
        NetworkMessage nm2 = new NetworkMessage(msg2, NetworkMessage.Type.REQUEST);
        NetworkMessage nm3 = new NetworkMessage(msg1, NetworkMessage.Type.NO_REPLY);
        NetworkMessage nm4 = new NetworkMessage(randomString(), NetworkMessage.Type.REQUEST);

        assert nm1.equals(nm2) && nm2.equals(nm1);
        assert !nm1.equals(nm3) && !nm3.equals(nm1);
        assert !nm1.equals(nm4) && !nm4.equals(nm1);
    }

    private String randomString() {
        return new BigInteger(130, random).toString(32);
    }
}
