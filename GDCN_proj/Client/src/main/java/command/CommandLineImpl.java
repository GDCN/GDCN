package command;

import net.tomp2p.storage.Data;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;

/**
 * Created by Leif on 2014-02-17.
 */
public class CommandLineImpl implements CommandLine {

    private boolean connected = false;

    private CmdNode node = null;

    public CommandLineImpl(){

    }

    @Override
    public void start(int port){

        //TODO report error
        if(isConnected()){
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
    public void bootstrap(String host, int port){
        //TODO report error
        if(!isConnected()){
            return;
        }

        //TODO implement
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
    public void discover2(int port) {
        node.discover2(port, new Listener<String>() {
            @Override
            public void message(boolean success, String message) {
                System.out.println(message);
            }
        });
    }

    @Override
    public void discover() {
        node.discover(new Listener<String>() {
            @Override
            public void message(boolean success, String message) {
                System.out.println(message);
            }
        });
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

}
