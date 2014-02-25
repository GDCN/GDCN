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
public class ClientImplementation implements ClientInput {

    private boolean connected = false;

    private CmdNode node = null;
    private ClientOutput output = null;

    private List<PeerAddress> peers;

    public ClientImplementation(){

    }

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
    public void put(String name, Data data) {
        Listener<String> listener = new Listener<String>() {
            @Override
            public void message(boolean success, String message) {
                System.out.println(message);
            }
        };
        node.put(name, data, listener);
    }

    @Override
    public void get(String name) {
        node.get(name, new Listener<Data>() {
            @Override
            public void message(boolean success, Data message) {
                try {
                    System.out.println(message.getObject().toString());
                } catch (ClassNotFoundException e) {
                    e.printStackTrace();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    @Override
    public void getNeighbors() {

        if(!isConnected()){
            System.out.println("Not connected!");
            return;
        }

        peers = node.getNeighbors( new Listener<String>() {

            @Override
            public void message(boolean success, String message) {
                System.out.println(message);
            }
        });
    }

    public void reBootstrap() {

        if(!isConnected()){
            System.out.println("Not connected!");
            return;
        }

        node.reBootstrap(peers, new Listener<String>() {
            @Override
            public void message(boolean success, String message) {
                System.out.println(message);
            }
        });
    }
}



