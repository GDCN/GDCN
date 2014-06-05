package unitTests;

import se.chalmers.gdcn.network.Crypto;
import network.Handshake;
import org.testng.annotations.Test;

import java.security.KeyPairGenerator;
import java.security.PublicKey;

/**
 * Created by weeeeeew on 2014-04-10.
 */
public class HandshakeTest {
    private final KeyPairGenerator rsaKeygen,dhKeygen;

    //TODO why use constructor? Standard is to use @BeforeClass and/or @BeforeMethod setup method.
    public HandshakeTest() throws Exception {
        rsaKeygen = KeyPairGenerator.getInstance("RSA");
        dhKeygen = KeyPairGenerator.getInstance(Crypto.AGREEMENT_ALGORITHM);
    }

    @Test
    public void testReply() {
        Handshake hs1 = new Handshake(dhKeygen.generateKeyPair().getPublic(), rsaKeygen.generateKeyPair().getPublic());
        Handshake hs2 = hs1.reply(dhKeygen.generateKeyPair().getPublic(), rsaKeygen.generateKeyPair().getPublic());

        assert hs2 != null;
        assert !hs1.equals(hs2);
        assert hs2.reply(dhKeygen.generateKeyPair().getPublic(), rsaKeygen.generateKeyPair().getPublic()) == null;
    }

    @Test
    public void testPublicKey() {
        PublicKey rsaKey1 = rsaKeygen.generateKeyPair().getPublic();
        PublicKey dhKey1 = dhKeygen.generateKeyPair().getPublic();

        Handshake hs = new Handshake(dhKey1,rsaKey1);

        assert hs.agreementKey.equals(dhKey1) && hs.signKey.equals(rsaKey1);

        PublicKey rsaKey2 = rsaKeygen.generateKeyPair().getPublic();
        PublicKey dhKey2 = dhKeygen.generateKeyPair().getPublic();

        Handshake hs2 = hs.reply(dhKey2,rsaKey2);

        assert hs2.signKey.equals(rsaKey2) && hs2.agreementKey.equals(dhKey2);
        assert !hs2.signKey.equals(rsaKey1) && !hs2.agreementKey.equals(dhKey1);
    }

    @Test
    public void testEquality() {
        PublicKey dhKey1 = dhKeygen.generateKeyPair().getPublic();
        PublicKey rsaKey1 = rsaKeygen.generateKeyPair().getPublic();

        Handshake hs1 = new Handshake(dhKey1,rsaKey1);
        Handshake hs2 = new Handshake(dhKey1,rsaKey1);

        assert hs1.equals(hs2) && hs2.equals(hs1);
        assert !hs1.equals(hs1.reply(dhKey1,rsaKey1)) && !hs1.reply(dhKey1,rsaKey1).equals(hs1);

        PublicKey dhKey2 = dhKeygen.generateKeyPair().getPublic();
        PublicKey rsaKey2 = rsaKeygen.generateKeyPair().getPublic();

        Handshake hs3 = new Handshake(dhKey1,rsaKey2);
        Handshake hs4 = new Handshake(dhKey2,rsaKey1);
        Handshake hs5 = new Handshake(dhKey2,rsaKey2);

        assert !hs1.equals(hs3) && !hs3.equals(hs1);
        assert !hs1.equals(hs4) && !hs4.equals(hs1);
        assert !hs1.equals(hs5) && !hs5.equals(hs1);
    }
}
