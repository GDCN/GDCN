package command;

import net.tomp2p.storage.Data;

import java.util.List;

/**
 * Created by HalfLeif on 2014-02-19.
 */
public interface CommandLine {
    boolean isConnected();

    void stop();

    void discover2(int port);

    void discover();

    void put(String name, Data data);

    void get(String name);
}
