package command;

import net.tomp2p.storage.Data;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

/**
 * Created by Leif on 2014-02-17.
 */
public class CommandLineImpl implements CommandLine {

    private boolean connected = true;


    private CmdNode node;

    public CommandLineImpl(int port){
        try {
            node = new CmdNode(port);
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    @Override
    public boolean isConnected() {
        return connected;
    }

    @Override
    public void stop(List<String> args) {
        node.shutdown();
        connected = false;
    }

    @Override
    public void discover2(List<String> args) {
        Integer port = Integer.parseInt(args.get(0));
        node.discover2(port, new Listener<String>() {
            @Override
            public void message(boolean success, String message) {
                System.out.println(message);
            }
        });
    }

    @Override
    public void discover(List<String> args) {
        node.discover(new Listener<String>() {
            @Override
            public void message(boolean success, String message) {
                System.out.println(message);
            }
        });
    }

    @Override
    public void put(List<String> args) {
        String name = args.remove(0);
        Listener<String> listener = new Listener<String>() {
            @Override
            public void message(boolean success, String message) {
                System.out.println(message);
            }
        };
        try {
            node.put(name, new Data(args), listener);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void get(List<String> args) {
        String name = args.remove(0);
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
