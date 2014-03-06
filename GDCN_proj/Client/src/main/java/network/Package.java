package network;

import java.io.Serializable;

/**
 * Created by HalfLeif on 2014-03-06.
 */
public class Package implements Serializable{
    private final int checksum;
    private final byte[] data;

    public Package(int checksum, byte[] data) {
        this.checksum = checksum;
        this.data = data;
    }

    public int getChecksum() {
        return checksum;
    }

    public byte[] getData() {
        return data;
    }
}
