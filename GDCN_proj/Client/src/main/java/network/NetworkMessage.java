package network;

/**
 * Created by Leif on 2014-03-24.
 */
public class NetworkMessage {

    private final Object object;
    private final Type type;

    public NetworkMessage(Object object, Type type) {
        this.object = object;
        this.type = type;
    }

    public Object getObject() {
        return object;
    }

    public Type getType() {
        return type;
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
