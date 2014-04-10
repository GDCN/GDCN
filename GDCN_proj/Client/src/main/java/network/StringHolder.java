package network;

/**
 * Class used for holding a String. Not really good architecture but it works.
 */
public class StringHolder {
    private String string = null;

    public synchronized String getString() {
        return string;
    }

    public synchronized void setString(String string) {
        this.string = string;
    }
}
