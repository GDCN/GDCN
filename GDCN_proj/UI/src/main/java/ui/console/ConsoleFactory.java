package ui.console;

import command.communicationToUI.ClientInput;
import net.tomp2p.storage.Data;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by Leif on 2014-02-25.
 */
public class ConsoleFactory {

    static Map<String, Command> createCommands(final ClientInput client, final Console console){
        Map<String, Command> commandMap = new HashMap<String, Command>();

        commandMap.put("exit", new Command() {
            @Override
            public void execute(List<String> args) {
                if(client.isConnected()){
                    System.out.println("You must run stop before you can exit!");
                    return;
                }
                console.stop();
            }
        });

        commandMap.put("start", new Command() {
            @Override
            public void execute(List<String> args) {
                int port = 4001;
                if(args.size()==1){
                    port=Integer.parseInt(args.get(0));
                }
                client.start(port);
            }
        });

        commandMap.put("stop", new Command() {
            @Override
            public void execute(List<String> args) {
                client.stop();
            }
        });

        commandMap.put("put", new Command() {
            @Override
            public void execute(List<String> args) {
                String name = args.remove(0);
                Data data = null;
                try {
                    data = new Data(args);
                    client.put(name, data);
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        });

        commandMap.put("get", new Command() {
            @Override
            public void execute(List<String> args) {
                client.get(args.remove(0));
            }
        });

        commandMap.put("connect", new Command() {
            @Override
            public void execute(List<String> args) {
                String site = "narrens.olf.sgsnet.se";
                if(args.size() == 1){
                    site = args.get(0);
                }

                try {
                    InetAddress[] inetAddresses = InetAddress.getAllByName(site);
                    for(InetAddress address : inetAddresses){

                        System.out.println(""+address.toString());
                    }
                } catch (UnknownHostException e) {
                    System.out.println("Unknown host: "+site);
                    e.printStackTrace();
                }
            }
        });

        commandMap.put("bootstrap", new Command() {
            @Override
            public void execute(List<String> args) {

                String host = "narrens.olf.sgsnet.se";
                int port = 4001;

                if(args.size()==0){
                    host = "narrens.olf.sgsnet.se";
                    port = 4001;
                } else if(args.size()==2){
                    host = args.get(0);
                    port = Integer.parseInt(args.get(1));
                } else {
                    System.out.println("Must take two arguments! Host and Port");
                    return;
                }
                client.bootstrap(host, port);
            }
        });

        commandMap.put("neighbors", new Command() {
            @Override
            public void execute(List<String> args) {
                client.getNeighbors();
            }
        });

        commandMap.put("rebootstrap", new Command() {
            @Override
            public void execute(List<String> args) {
                client.reBootstrap();
            }
        });


        //As long as the help command is done in this way it needs to be at the bottom
        final Set<String> commands = commandMap.keySet();

        commandMap.put("help", new Command() {
            @Override
            public void execute(List<String> args) {
                System.out.println(commands.toString());
            }
        });


        return commandMap;
    }
}
