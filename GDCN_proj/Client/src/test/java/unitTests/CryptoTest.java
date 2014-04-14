package unitTests;

import network.Crypto;
import org.testng.annotations.Test;

import javax.crypto.BadPaddingException;
import javax.crypto.SealedObject;
import java.io.Serializable;
import java.math.BigInteger;
import java.security.*;
import java.util.Random;

/**
 * Created by weeeeeew on 2014-04-10.
 */
public class CryptoTest {
    private KeyPairGenerator keygen;
    private Random random;

    public CryptoTest() throws Exception {
        keygen = KeyPairGenerator.getInstance("RSA");
        random = new Random();
    }

    @Test
     public void testEncryptDecrypt() throws Exception {
        KeyPair keypair = keygen.generateKeyPair();
        String message = randomString();
        SealedObject encMsg = Crypto.encrypt(message,keypair.getPublic());

        assert message.equals((String) Crypto.decrypt(encMsg,keypair.getPrivate()));

        PrivateKey wrongKey = keygen.generateKeyPair().getPrivate();

        assert Crypto.decrypt(encMsg,wrongKey) == null;
    }

    @Test
    public void testSignVerify() throws Exception {
        KeyPair keypair = keygen.generateKeyPair();
        SignedObject signedObject = Crypto.sign(randomString(), keypair.getPrivate());

        assert Crypto.verify(signedObject, keypair.getPublic());

        PublicKey wrongKey = keygen.generateKeyPair().getPublic();

        assert !Crypto.verify(signedObject, wrongKey);
    }

    @Test
    public void testSignedCorrectObject() throws Exception {
        PrivateKey privateKey = keygen.generateKeyPair().getPrivate();
        String message = randomString();
        SignedObject signedObject = Crypto.sign(message, privateKey);

        assert message.equals((String) signedObject.getObject());
    }

    @Test
    public void testSignEncrypt() throws Exception {
        KeyPair signKeys = keygen.generateKeyPair();
        KeyPair encryptKeys = keygen.generateKeyPair();
        String msg = randomString();

        SealedObject sealedObject = Crypto.signAndEncrypt(msg, signKeys.getPrivate(), encryptKeys.getPublic());
        Serializable decrypted = Crypto.decryptAndVerify(sealedObject, encryptKeys.getPrivate(), signKeys.getPublic());

        assert msg.equals(decrypted);
    }


    private String randomString() {
        return new BigInteger(130, random).toString(32);
    }
}
