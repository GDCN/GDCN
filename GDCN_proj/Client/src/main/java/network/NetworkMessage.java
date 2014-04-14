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
        try {
            return Crypto.signAndEncrypt(this,myKey,otherKey);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public static NetworkMessage decryptAndVerify(SealedObject sealedData, PrivateKey myKey, PublicKey otherKey) throws ClassNotFoundException, SignatureException, InvalidKeyException, IOException {
        Serializable data = null;
        try {
            data = Crypto.decryptAndVerify(sealedData, myKey, otherKey);
        } catch (Exception e) {
            e.printStackTrace();
        }

        if (data instanceof NetworkMessage) {
            return (NetworkMessage) data;
        } else {
            throw new InvalidParameterException("The encrypted object was not a NetworkMessage");
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

    @Override
    public boolean equals(Object other) {
        return other instanceof NetworkMessage ? this.object.equals( ((NetworkMessage) other).object ) && this.type == ((NetworkMessage) other).type : false;
    }

}
