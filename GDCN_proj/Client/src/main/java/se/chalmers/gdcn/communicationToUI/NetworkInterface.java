package se.chalmers.gdcn.communicationToUI;

import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;

import java.beans.PropertyChangeListener;
import java.util.List;

/**
 * Created by HalfLeif on 2014-03-11.
 */
public interface NetworkInterface {

    void addListener(PropertyChangeListener listener);

    void removeListener(PropertyChangeListener listener);

    void start(int port);

    void stop();

    void bootstrap(String host, int port);

    void bootstrap();

    void put(String name, Data value);

    void put(Number160 key, Data value);

    void get(String name);

    void get(Number160 key);

    List<PeerAddress> getNeighbours();

    List<PeerAddress> getOldNeighbours();

    void reBootstrap();

    void send(String msg);

    void put(Number160 key, Number160 domain, Data value);

    void get(Number160 key, Number160 domain);

    void setNeighbourFile(String file);

    void clearNeighbourFile();

    void deleteNeighbourFile();

    void requestWork(int index);

    Number160 getID();

}
