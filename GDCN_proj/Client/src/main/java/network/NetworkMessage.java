package network;

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
