package command;

import net.tomp2p.storage.Data;

/**
 * Created by Leif on 2014-02-25.
 */
public interface ClientOutput {
    void started(boolean success, int port, String errorMessage);

    void bootstrapped(boolean success, String host, int port, String errorMessage);

    void stopped(boolean success, String errorMessage);

    void put(boolean success, String name, Data data, String errorMessage);

    void got(boolean success, String name, Data data, String errorMessage);

    void gotNeighbors(boolean success, String errorMessage);

    void reBootstrapped(boolean success, String errorMessage);
}
