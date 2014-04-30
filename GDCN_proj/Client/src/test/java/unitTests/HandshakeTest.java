package unitTests;

import network.Crypto;
import network.Handshake;
import org.testng.annotations.Test;

import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PublicKey;
import java.security.SignedObject;

/**
 * Created by weeeeeew on 2014-04-10.
 */
public class HandshakeTest {
    private final KeyPairGenerator keygen;

    public HandshakeTest() throws Exception {
        keygen = KeyPairGenerator.getInstance("RSA");
    }

    @Test
    public void testReply() {
        Handshake hs = new Handshake(keygen.generateKeyPair().getPublic());

        assert hs.reply(keygen.generateKeyPair().getPublic()) != null;
    }

    @Test
    public void testDoubleReply() {
        Handshake hs1 = new Handshake(keygen.generateKeyPair().getPublic());
        Handshake hs2 = hs1.reply(keygen.generateKeyPair().getPublic());

        assert hs2.reply(keygen.generateKeyPair().getPublic()) == null;
    }

    @Test
    public void testPublicKey() {
        PublicKey key = keygen.generateKeyPair().getPublic();
        Handshake hs = new Handshake(key);

        assert hs.dhKey.equals(key);
    }

    @Test
    public void testReplyPublicKey() {
        PublicKey key1 = keygen.generateKeyPair().getPublic();
        PublicKey key2 = keygen.generateKeyPair().getPublic();
        Handshake hs1 = new Handshake(key1);
        Handshake hs2 = hs1.reply(key2);

        assert hs2.dhKey.equals(key2);
    }

    @Test
    public void testSignatureValid() throws Exception {
        PublicKey key = keygen.generateKeyPair().getPublic();
        KeyPair keypair = keygen.generateKeyPair();
        Handshake hs = new Handshake(key);
        SignedObject signedHS = hs.sign(keypair.getPrivate());

        assert Crypto.verify(signedHS,keypair.getPublic());
    }

    @Test
    public void testSignedObject() throws Exception {
        KeyPair keypair = keygen.generateKeyPair();
        Handshake hs = new Handshake(keypair.getPublic());
        SignedObject signedHS = hs.sign(keypair.getPrivate());

        assert hs.equals((Handshake) signedHS.getObject());
    }
}
