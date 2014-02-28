package ui.console;

import command.communicationToUI.ClientInterface;
import command.communicationToUI.CommandWord;
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

    public static Console create(final ClientInterface client){
        Map<String, Command> commandMap = ConsoleFactory.createCommands(client);
        Console console = new Console(commandMap);
        client.addListener(console);

        return console;
    }

    private static Map<String, Command> createCommands(final ClientInterface client){
        Map<String, Command> commandMap = new HashMap<String, Command>();

        commandMap.put(CommandWord.START.getName(), new Command() {
            @Override
            public void execute(List<String> args) {
                int port = 4001;
                if(args.size()==1){
                    port=Integer.parseInt(args.get(0));
                }
                client.start(port);
            }
        });

        commandMap.put(CommandWord.STOP.getName(), new Command() {
            @Override
            public void execute(List<String> args) {
                client.stop();
            }
        });

        commandMap.put(CommandWord.PUT.getName(), new Command() {
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

        commandMap.put(CommandWord.GET.getName(), new Command() {
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

        commandMap.put(CommandWord.BOOTSTRAP.getName(), new Command() {
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

        commandMap.put(CommandWord.WORK.getName(), new Command() {
            @Override
            public void execute(List<String> args) {
                String moduleName = args.get(0);
                String initData = args.get(1);
                //TODO
                client.work(moduleName+"_1", moduleName, initData);
            }
        });

        //TODO use enum
        commandMap.put("neighbours", new Command() {
            @Override
            public void execute(List<String> args) {
                client.getNeighbours();
            }
        });

        commandMap.put("rebootstrap", new Command() {
            @Override
            public void execute(List<String> args) {
                //TODO fix!
//                client.reBootstrap();
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
