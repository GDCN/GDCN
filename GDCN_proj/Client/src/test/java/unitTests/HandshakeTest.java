package unitTests;

import network.Crypto;
import network.Handshake;
import org.testng.annotations.Test;

import javax.crypto.interfaces.DHPublicKey;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PublicKey;
import java.security.SignedObject;
import java.security.interfaces.RSAPublicKey;

/**
 * Created by weeeeeew on 2014-04-10.
 */
public class HandshakeTest {
    private final KeyPairGenerator rsaKeygen,dhKeygen;

    public HandshakeTest() throws Exception {
        rsaKeygen = KeyPairGenerator.getInstance("RSA");
        dhKeygen = KeyPairGenerator.getInstance(Crypto.AGREEMENT_ALGORITHM);
    }

    @Test
    public void testReply() {
        Handshake hs1 = new Handshake((DHPublicKey) dhKeygen.generateKeyPair().getPublic(), (RSAPublicKey) rsaKeygen.generateKeyPair().getPublic());
        Handshake hs2 = hs1.reply((DHPublicKey) dhKeygen.generateKeyPair().getPublic(), (RSAPublicKey) rsaKeygen.generateKeyPair().getPublic());

        assert hs2 != null;
    }

    @Test
    public void testDoubleReply() {
        Handshake hs1 = new Handshake(rsaKeygen.generateKeyPair().getPublic());
        Handshake hs2 = hs1.reply(rsaKeygen.generateKeyPair().getPublic());

        assert hs2.reply(rsaKeygen.generateKeyPair().getPublic()) == null;
    }

    @Test
    public void testPublicKey() {
        PublicKey key = rsaKeygen.generateKeyPair().getPublic();
        Handshake hs = new Handshake(key);

        assert hs.dhKey.equals(key);
    }

    @Test
    public void testReplyPublicKey() {
        PublicKey key1 = rsaKeygen.generateKeyPair().getPublic();
        PublicKey key2 = rsaKeygen.generateKeyPair().getPublic();
        Handshake hs1 = new Handshake(key1);
        Handshake hs2 = hs1.reply(key2);

        assert hs2.dhKey.equals(key2);
    }

    @Test
    public void testSignatureValid() throws Exception {
        PublicKey key = rsaKeygen.generateKeyPair().getPublic();
        KeyPair keypair = rsaKeygen.generateKeyPair();
        Handshake hs = new Handshake(key);
        SignedObject signedHS = hs.sign(keypair.getPrivate());

        assert Crypto.verify(signedHS,keypair.getPublic());
    }

    @Test
    public void testSignedObject() throws Exception {
        KeyPair keypair = rsaKeygen.generateKeyPair();
        Handshake hs = new Handshake(keypair.getPublic());
        SignedObject signedHS = hs.sign(keypair.getPrivate());

        assert hs.equals((Handshake) signedHS.getObject());
    }
}
