package command;

/**
 * Created by Leif on 2014-02-17.
 */
public interface Listener<T> {
    public void message(boolean success, T message);
}
