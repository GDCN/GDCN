package command.communicationToUI;

import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;

import java.beans.PropertyChangeListener;
import java.util.List;

/**
 * Created by HalfLeif on 2014-02-26.
 */
public interface ClientInterface {
    void addListener(PropertyChangeListener listener);

    void removeListener(PropertyChangeListener listener);

    void start(int port);

    void stop();

    void bootstrap(String host, int port);

    void put(String name, Data value);

    void get(String name);

    void work(String projectName, String taskName);

    List<PeerAddress> getNeighbours();

    void reBootstrap(List<PeerAddress> peers);
}
