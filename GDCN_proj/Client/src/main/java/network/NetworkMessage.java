package network;

import java.io.Serializable;

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

    /**
     * Encrypt message using receiving peer's public key
     * @return encrypted message
     */
    public Object encrypt(){
        //TODO encrypt and sign message
//        try {
//            return new Data(this);
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
        return this;
    }

    /**
     * Decrypt message using private key
     * @param data Encrypted message
     * @return Decrypted message
     */
    public static NetworkMessage decrypt(Object data){
//        //TODO decrypt message
//        try {
//            return (NetworkMessage) data.getObject();
//        } catch (ClassNotFoundException e) {
//            e.printStackTrace();
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
        return (NetworkMessage) data;
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
