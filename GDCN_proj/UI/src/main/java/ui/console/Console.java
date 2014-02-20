package ui.console;

import command.CommandLine;
import command.CommandLineImpl;
import net.tomp2p.storage.Data;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.*;

/**
 * Created by HalfLeif on 2014-02-19.
 */
public class Console {

    private final Holder commandHolder;
    private CommandLine commandLine = null;

    private boolean loop = true;

    public Console() {
        commandHolder = new Holder(createCommands());
        commandLine = new CommandLineImpl();
    }

    public static void main(String[] args){
        Console console = new Console();
        console.read();
    }

    public void read(){
        BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(System.in));
        try {

            while(loop){
                System.out.print(">> ");
                String line = bufferedReader.readLine();
                String[] words = line.split("\\s");
                List<String> wordList = new ArrayList<String>(Arrays.asList(words));
                String cmd = wordList.remove(0);
                try{
                    commandHolder.execute(cmd, wordList);
                } catch (UnsupportedOperationException e){
                    System.out.println("Unsupported operation ("+cmd+"). Type \"exit\" to stop");
                }

            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                bufferedReader.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private Map<String, Command> createCommands(){
        Map<String, Command> commandMap = new HashMap<String, Command>();

        commandMap.put("exit", new Command() {
            @Override
            public void execute(List<String> args) {
                if(commandLine.isConnected()){
                    System.out.println("You must run stop before you can exit!");
                    return;
                }
                loop = false;
            }
        });

        commandMap.put("start", new Command() {
            @Override
            public void execute(List<String> args) {
                int port = 4001;
                if(args.size()==1){
                    port=Integer.parseInt(args.get(0));
                }
                commandLine.start(port);
            }
        });

        commandMap.put("stop", new Command() {
            @Override
            public void execute(List<String> args) {
                commandLine.stop();
            }
        });

        commandMap.put("discover2", new Command() {
            @Override
            public void execute(List<String> args) {
                int port = Integer.parseInt(args.get(0));
                commandLine.discover2(port);
            }
        });

        commandMap.put("discover", new Command() {
            @Override
            public void execute(List<String> args) {
                commandLine.discover();
            }
        });

        commandMap.put("put", new Command() {
            @Override
            public void execute(List<String> args) {
                String name = args.remove(0);
                Data data = null;
                try {
                    data = new Data(args);
                    commandLine.put(name, data);
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        });

        commandMap.put("get", new Command() {
            @Override
            public void execute(List<String> args) {
                commandLine.get(args.remove(0));
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

                } else if(args.size()==2){
                    host = args.get(0);
                    port = Integer.parseInt(args.get(1));
                } else {
                    System.out.println("Must take two arguments! Host and Port");
                    return;
                }
                commandLine.bootstrap(host, port);
            }
        });

        return commandMap;
    }
}
