package unitTests;

import network.Crypto;
import org.testng.annotations.Test;

import javax.crypto.BadPaddingException;
import javax.crypto.SealedObject;
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
    }

    @Test
    public void testSignVerify() throws Exception {
        KeyPair keypair = keygen.generateKeyPair();
        SignedObject signedObject = Crypto.sign(randomString(), keypair.getPrivate());

        assert Crypto.verify(signedObject, keypair.getPublic());
    }

    @Test(expectedExceptions = BadPaddingException.class)
    public void testWrongDecryption() throws Exception {
        PublicKey publicKey = keygen.generateKeyPair().getPublic();
        PrivateKey wrongPrivateKey = keygen.generateKeyPair().getPrivate();
        SealedObject encMsg = Crypto.encrypt(randomString(), publicKey);
        Crypto.decrypt(encMsg, wrongPrivateKey);
    }

    @Test
    public void testWrongVerification() throws Exception {
        PrivateKey privateKey = keygen.generateKeyPair().getPrivate();
        PublicKey wrongPublicKey = keygen.generateKeyPair().getPublic();
        SignedObject signedObject = Crypto.sign(randomString(), privateKey);

        assert !Crypto.verify(signedObject,wrongPublicKey);
    }

    @Test
    public void testSignedCorrectObject() throws Exception {
        PrivateKey privateKey = keygen.generateKeyPair().getPrivate();
        String message = randomString();
        SignedObject signedObject = Crypto.sign(message, privateKey);

        assert message.equals((String) signedObject.getObject());
    }


    private String randomString() {
        return new BigInteger(130, random).toString(32);
    }
}
