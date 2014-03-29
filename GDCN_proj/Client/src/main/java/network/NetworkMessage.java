package network;

import net.tomp2p.storage.Data;

import java.io.IOException;
import java.io.Serializable;

/**
 * Created by Leif on 2014-03-24.
 */
public class NetworkMessage implements Serializable {

    private final Object object;
    private final Type type;
    private final Long ref;

    public NetworkMessage(Object object, Type type, Long ref) {
        this.object = object;
        this.type = type;
        this.ref = ref;
    }

    public Object getObject() {
        return object;
    }

    public Type getType() {
        return type;
    }

    public Long getRef() {
        return ref;
    }

    public Data encrypt(){
        //TODO encrypt and sign message
        try {
            return new Data(this);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static NetworkMessage decrpyt(Data data){
        //TODO decrypt message
        try {
            return (NetworkMessage) data.getObject();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public String toString() {
        return "NetworkMessage{" +
                "object=" + object +
                ", type=" + type +
                '}';
    }

    public static enum Type {
        OK,
        REQUEST,
        NO_REPLY
    }


}
