package network;

import java.io.IOException;
import java.io.Serializable;
import java.security.*;

/**
 * Created by weeeeeew on 2014-04-10.
 */
public class Handshake implements Serializable {
    public final PublicKey publicKey;
    private final Stage stage;

    public Handshake(PublicKey publicKey) {
        this(publicKey,Stage.INIT);
    }

    private Handshake(PublicKey publicKey, Stage stage) {
        this.publicKey = publicKey;
        this.stage = stage;
    }

    public SignedObject sign(PrivateKey key) throws InvalidKeyException, IOException, SignatureException {
        return Crypto.sign(this,key);
    }

    public Handshake reply(PublicKey key) {
        return stage == Stage.INIT ? new Handshake(key,Stage.REPLY) : null;
    }

    private enum Stage {
        INIT,
        REPLY
    }
}
