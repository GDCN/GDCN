package unitTests;

import network.Crypto;
import org.testng.annotations.Test;

import javax.crypto.KeyAgreement;
import javax.crypto.KeyGenerator;
import javax.crypto.SealedObject;
import javax.crypto.SecretKey;
import java.math.BigInteger;
import java.security.*;
import java.util.Random;

/**
 * Created by weeeeeew on 2014-04-10.
 */
public class CryptoTest {
    private KeyGenerator secretKeygen;
    private KeyPairGenerator publicKeygen;
    private Random random;

    //TODO why use constructor? Standard is to use @BeforeClass and/or @BeforeMethod setup method.
    public CryptoTest() throws Exception {
        secretKeygen = KeyGenerator.getInstance(Crypto.SECRET_KEY_ALGORITHM);
        publicKeygen = KeyPairGenerator.getInstance(Crypto.PUBLIC_KEY_ALGORITHM);
        random = new Random();
        //TODO give seed. Tests should be fully deterministic even when using Random. can run same test multiple times instead.
    }

    @Test
     public void testEncryptDecrypt() throws Exception {
        SecretKey key = secretKeygen.generateKey();
        String message = randomString();
        SealedObject encMsg = Crypto.encrypt(message, key);

        assert message.equals(Crypto.decrypt(encMsg,key));

        SecretKey wrongKey = secretKeygen.generateKey();

        assert Crypto.decrypt(encMsg,wrongKey) == null;
    }

    @Test
    public void testSignVerify() throws Exception {
        KeyPair keypair = publicKeygen.generateKeyPair();
        SignedObject signedObject = Crypto.sign(randomString(), keypair.getPrivate());

        assert Crypto.verify(signedObject, keypair.getPublic());

        PublicKey wrongKey = publicKeygen.generateKeyPair().getPublic();

        assert !Crypto.verify(signedObject, wrongKey);
    }

    @Test
    public void testSignedCorrectObject() throws Exception {
        PrivateKey privateKey = publicKeygen.generateKeyPair().getPrivate();
        String message = randomString();
        SignedObject signedObject = Crypto.sign(message, privateKey);

        assert message.equals(signedObject.getObject());
    }


    @Test
    public void testAgreement() throws Exception {
        KeyAgreement backupAgreement = KeyAgreement.getInstance(Crypto.AGREEMENT_ALGORITHM);
        KeyPairGenerator backupKeygen = KeyPairGenerator.getInstance(Crypto.AGREEMENT_ALGORITHM);
        KeyPair keyPair1 = Crypto.generateDHKeyPair();
        KeyPair keyPair2 = backupKeygen.generateKeyPair();

        assert !keyPair1.equals(keyPair2);

        SecretKey secretKey1 = Crypto.generateSecretKey(keyPair1.getPrivate(),keyPair2.getPublic());

        backupAgreement.init(keyPair2.getPrivate());
        backupAgreement.doPhase(keyPair1.getPublic(),true);

        SecretKey secretKey2 = backupAgreement.generateSecret(Crypto.SECRET_KEY_ALGORITHM);

        assert secretKey1.equals(secretKey2);
    }

    private String randomString() {
        return new BigInteger(130, random).toString(32);
    }
}
