package command;

import net.tomp2p.storage.Data;

/**
 * Created by HalfLeif on 2014-02-19.
 */
public interface ClientInput {
    void start(int port);

    void bootstrap(String host, int port);

    boolean isConnected();

    void stop();

    void put(String name, Data data);

    void get(String name);

    void getNeighbors();

    void reBootstrap();
}
