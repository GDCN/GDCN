package ui.console;

import command.communicationToUI.ClientInterface;
import command.communicationToUI.CommandWord;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by Leif on 2014-02-25.
 */
public class ConsoleFactory {

    /**
     * Creates a console and makes it listen to the provided client.
     * @param client
     * @return
     */
    public static Console create(final ClientInterface client){
        Map<String, Command> commandMap = ConsoleFactory.createCommands(client);
        Console console = new Console(commandMap);
        client.addListener(console);

        return console;
    }

    /**
     * Creates and encapsulates commands
     * @param client Client to pass commands to
     * @return
     */
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

        //TODO remove command or make better (enum + good output)
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

        //TODO keep or remove?
        commandMap.put("send", new Command() {
            @Override
            public void execute(List<String> args) {
                client.send(args.get(0));
            }
        });

        //TODO keep or remove?
        commandMap.put("sendd", new Command() {
            @Override
            public void execute(List<String> args) {
                client.sendd(args.get(0));
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
                //TODO check num args

                String projectName = args.get(0);
                String taskName = args.get(1);
                client.work(projectName, taskName);
            }
        });

        commandMap.put(CommandWord.INSTALL.getName(), new Command() {
            @Override
            public void execute(List<String> args) {
                client.install();
            }
        });

        commandMap.put(CommandWord.UNINSTALL.getName(), new Command() {
            @Override
            public void execute(List<String> args) {
                client.uninstall();
            }
        });

        commandMap.put(CommandWord.PUSH.getName(), new Command() {
            @Override
            public void execute(List<String> args) {
                String jobName = args.get(0);
                client.push(jobName);
            }
        });

        //TODO use enum
        commandMap.put("neighbours", new Command() {
            @Override
            public void execute(List<String> args) {
                List<PeerAddress> neighbours = client.getNeighbours();
                System.out.println("NEIGHBOURS:");
                for(PeerAddress address : neighbours){
                    System.out.println(address.toString());
                }
            }
        });

        //TODO use enum
        commandMap.put("put2", new Command() {
            @Override
            public void execute(List<String> args) {
                String key = args.get(0);
                String domain = args.get(1);
                String value = args.get(2);
                client.put2(key, domain, value);
            }
        });

        //TODO use enum
        commandMap.put("get2", new Command() {
            @Override
            public void execute(List<String> args) {
                String key = args.get(0);
                String domain = args.get(1);
                client.get2(key, domain);
            }
        });

        //TODO use enum
        commandMap.put("rebootstrap", new Command() {
            @Override
            public void execute(List<String> args) {
                //TODO fix!
//                client.reBootstrap();
            }
        });

        return commandMap;
    }
}
