package se.chalmers.gdcn.ui;

import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.communicationToUI.CommandWord;
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
        Map<String, UICommand> commandMap = ConsoleFactory.createCommands(client);
        Console console = new Console(commandMap);
        client.addListener(console);

        return console;
    }

    /**
     * Creates and encapsulates commands
     * @param client Client to pass commands to
     * @return
     */
    private static Map<String, UICommand> createCommands(final ClientInterface client){
        Map<String, UICommand> commandMap = new HashMap<>();

        commandMap.put(CommandWord.START.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                int port = 4001;
                if(args.size()==1){
                    port=Integer.parseInt(args.get(0));
                }
                client.start(port);
            }
        });

        commandMap.put(CommandWord.STOP.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.stop();
            }
        });

        commandMap.put(CommandWord.PUT.getName(), new UICommand() {
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

        commandMap.put(CommandWord.GET.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.get(args.remove(0));
            }
        });

        //TODO remove command or make better (enum + good output)
        commandMap.put("connect", new UICommand() {
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
        commandMap.put("send", new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.send(args.get(0));
            }
        });

        commandMap.put(CommandWord.BOOTSTRAP.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {

                String host;
                int port;

                if(args.size()==0){
                    client.bootstrap();
                    System.out.println("bootstraping to bootstrap node");
                } else if(args.size()==2){
                    host = args.get(0);
                    port = Integer.parseInt(args.get(1));
                    System.out.println("connecting to other node");
                    client.bootstrap(host, port);
                } else {
                    System.out.println("Normally two arguments: Host and Port. Zero arguments for default bootstrap.");
                }
            }
        });

        commandMap.put(CommandWord.WORK.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {

                String address = "narrens.olf.sgsnet.se";
                int port = 4001;

                if(args.size()>1){
                    address = args.get(0);
                    port = Integer.parseInt(args.get(1));
                }

                client.work(address, port, false);
            }
        });

        commandMap.put(CommandWord.AUTO_WORK.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                String address = "narrens.olf.sgsnet.se";
                int port = 4001;

                if(args.size()>1){
                    address = args.get(0);
                    port = Integer.parseInt(args.get(1));
                }

                client.work(address, port, true);
            }
        });

        commandMap.put(CommandWord.INSTALL.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.install();
            }
        });

        commandMap.put(CommandWord.UNINSTALL.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.uninstall();
            }
        });

        commandMap.put(CommandWord.PUSH.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                String jobName = args.get(0);
                client.push(jobName);
            }
        });

        //TODO use enum
        commandMap.put("neighbours", new UICommand() {
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
        commandMap.put("put2", new UICommand() {
            @Override
            public void execute(List<String> args) {
                String key = args.get(0);
                String domain = args.get(1);
                String value = args.get(2);
                client.put2(key, domain, value);
            }
        });

        //TODO use enum
        commandMap.put("get2", new UICommand() {
            @Override
            public void execute(List<String> args) {
                String key = args.get(0);
                String domain = args.get(1);
                client.get2(key, domain);
            }
        });

        //TODO remove entirely? Currently asks neighbour for work.
        commandMap.put("reqw", new UICommand() {
            @Override
            public void execute(List<String> args) {
                int ix = 0;
                if(args.size()>0){
                    ix = Integer.parseInt(args.get(0));
                }
                client.requestWork(ix);
            }
        });

        //TODO use enum
        commandMap.put("rebootstrap", new UICommand() {
            @Override
            public void execute(List<String> args) {
                //TODO fix!
                client.reBootstrap();
            }
        });

        commandMap.put("clearNeighbourFile", new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.clearNeighbourFile();
            }
        });

        commandMap.put("deleteNeighbourFile", new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.deleteNeighbourFile();
            }
        });

        return commandMap;
    }
}
