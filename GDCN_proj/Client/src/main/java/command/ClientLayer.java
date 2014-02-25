package command;

import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;
import java.util.List;

/**
 * Created by Leif on 2014-02-17.
 */
public class ClientLayer implements ClientInput {

    private boolean connected = false;

    private CmdNode node = null;
    private ClientOutput output = null;

    private List<PeerAddress> peers;

    @Override
    public boolean isConnected() {
        return connected;
    }

    @Override
    public void stop() {
        node.shutdown();
        connected = false;
    }

    @Override
    public void start(int port){
        if(isConnected()){
            output.started(false, port,"Has a node already!");
            return;
        }

        try {
            node = new CmdNode(port);
            connected = true;
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void bootstrap(final String host, final int port){
        if(!isConnected()){
            output.bootstrapped(false, host, port, "Not connected!");
            return;
        }

        try {
            InetAddress inetAddress = InetAddress.getByName(host);
            node.bootstrap(inetAddress,port,new Listener<String>() {
                @Override
                public void message(boolean success, String message) {
                    output.bootstrapped(true, host, port, message);
                }
            });
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void put(final String name, final Data data) {
        node.put(name, data, new Listener<String>() {
            @Override
            public void message(boolean success, String message) {
                output.put(true, name, data, message);
            }
        });
    }

    @Override
    public void get(final String name) {
        node.get(name, new Listener<Data>() {
            @Override
            public void message(boolean success, Data message) {
                output.got(success, name, message, "");
            }
        });
    }

    @Override
    public void getNeighbors() {

        if(!isConnected()){
            output.gotNeighbors(false, "Not connected!");
            return;
        }

        peers = node.getNeighbors( new Listener<String>() {

            @Override
            public void message(boolean success, String message) {
                output.gotNeighbors(success, message);
            }
        });
    }

    public void reBootstrap() {

        if(!isConnected()){
            output.reBootstrapped(false, "Not connected!");
            return;
        }

        node.reBootstrap(peers, new Listener<String>() {
            @Override
            public void message(boolean success, String message) {
                output.reBootstrapped(success, message);
            }
        });
    }
}



