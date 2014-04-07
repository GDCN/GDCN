package network;

import javax.crypto.*;
import java.io.IOException;
import java.io.Serializable;
import java.security.*;

/**
 * Created by Leif on 2014-03-24.
 *
 * General purpose class used for any message passing.
 */
public class NetworkMessage implements Serializable {

    private final Serializable object;
    private final Type type;

    public NetworkMessage(Serializable object, Type type) {
        this.object = object;
        this.type = type;
    }

    public Object getObject() {
        return object;
    }

    public Type getType() {
        return type;
    }

    public SealedObject signAndEncrypt(PrivateKey myKey, PublicKey otherKey) throws InvalidKeyException, IOException, SignatureException {
        return Crypto.signAndEncrypt(this,myKey,otherKey);
    }

    public static NetworkMessage decryptAndVerify(SealedObject sealedData, PrivateKey myKey, PublicKey otherKey) throws Exception {
        Serializable data = Crypto.decryptAndVerify(sealedData,myKey,otherKey);

        if (data.getClass() == NetworkMessage.class) {
            return (NetworkMessage) data;
        } else {
            throw new Exception("Wrong class!");
        }
    }

    @Override
    public String toString() {
        return "NetwM{ " + type +
                ", " + object +
                '}';
    }

    public static enum Type {
        REQUEST,
        NO_REPLY
    }


}
