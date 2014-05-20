package se.chalmers.gdcn.ui;

import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.WordInterface;

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
     * @param client Client to pass commands to
     * @return Console instance that can take commands
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
     * @return Map of commands
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

            @Override
            public WordInterface getWord() {
                return CommandWord.START;
            }
        });

        commandMap.put(CommandWord.STOP.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.stop();
            }

            @Override
            public WordInterface getWord() {
                return CommandWord.STOP;
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

            @Override
            public WordInterface getWord() {
                return CommandWord.PUT;
            }
        });

        commandMap.put(CommandWord.GET.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.get(args.remove(0));
            }

            @Override
            public WordInterface getWord() {
                return CommandWord.GET;
            }
        });

        commandMap.put(CommandWord.BOOTSTRAP.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {

                String host;
                int port;

                if(args.size()==0){
                    client.bootstrap();
                } else if(args.size()==2){
                    host = args.get(0);
                    port = Integer.parseInt(args.get(1));
                    client.bootstrap(host, port);
                } else {
                    System.out.println("Normally two arguments: Host and Port. Zero arguments for default bootstrap.");
                }
            }

            @Override
            public WordInterface getWord() {
                return CommandWord.BOOTSTRAP;
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

            @Override
            public WordInterface getWord() {
                return CommandWord.WORK;
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

            @Override
            public WordInterface getWord() {
                return CommandWord.AUTO_WORK;
            }
        });

        commandMap.put(CommandWord.INSTALL.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.install();
            }

            @Override
            public WordInterface getWord() {
                return CommandWord.INSTALL;
            }
        });

        commandMap.put(CommandWord.UNINSTALL.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.uninstall();
            }

            @Override
            public WordInterface getWord() {
                return CommandWord.UNINSTALL;
            }
        });

        commandMap.put(CommandWord.PUSH.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                String jobName = args.get(0);
                client.push(jobName);
            }

            @Override
            public WordInterface getWord() {
                return CommandWord.PUSH;
            }
        });

        //////////////////////////////////
        // Debug or outdated commands:
        // TODO remove or improve them

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

            @Override
            public WordInterface getWord() {
                return nullWord;
            }
        });

        commandMap.put("send", new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.send(args.get(0));
            }

            @Override
            public WordInterface getWord() {
                return nullWord;
            }
        });

        commandMap.put("neighbours", new UICommand() {
            @Override
            public void execute(List<String> args) {
                List<PeerAddress> neighbours = client.getNeighbours();
                System.out.println("NEIGHBOURS:");
                for(PeerAddress address : neighbours){
                    System.out.println(address.toString());
                }
            }

            @Override
            public WordInterface getWord() {
                return nullWord;
            }
        });

        commandMap.put("rebootstrap", new UICommand() {
            @Override
            public void execute(List<String> args) {
                //TODO fix or delete!
                client.reBootstrap();
            }

            @Override
            public WordInterface getWord() {
                return nullWord;
            }
        });

        commandMap.put("clearNeighbourFile", new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.clearNeighbourFile();
            }

            @Override
            public WordInterface getWord() {
                return nullWord;
            }
        });

        commandMap.put("deleteNeighbourFile", new UICommand() {
            @Override
            public void execute(List<String> args) {
                client.deleteNeighbourFile();
            }

            @Override
            public WordInterface getWord() {
                return nullWord;
            }
        });

        return commandMap;
    }

    private static WordInterface nullWord = new WordInterface() {
        @Override
        public int getArity() {
            return 100;
        }

        @Override
        public String getName() {
            return "";
        }

        @Override
        public String getArguments() {
            return "";
        }

        @Override
        public String getHelp() {
            return "";
        }
    };
}
