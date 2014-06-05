package se.chalmers.gdcn.network;

import javax.crypto.SecretKey;
import java.io.Serializable;
import java.security.InvalidKeyException;
import java.security.InvalidParameterException;

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

    public byte[] encrypt(SecretKey key) throws InvalidKeyException {
        return Crypto.encrypt(this, key);
    }

    public static NetworkMessage decrypt(byte[] ciphertext, SecretKey key) throws InvalidKeyException {
        Serializable data = Crypto.decrypt(ciphertext, key);

        if (data instanceof NetworkMessage) {
            return (NetworkMessage) data;
        } else if (data == null) {
            return null;
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
    public boolean equals(Object o) {
        return o instanceof NetworkMessage
                ? this.object.equals( ((NetworkMessage) o).object ) && this.type == ((NetworkMessage) o).type
                : false;
    }

}
