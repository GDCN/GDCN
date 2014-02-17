package command;

import net.tomp2p.storage.Data;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.security.NoSuchAlgorithmException;
import java.util.*;

/**
 * Created by Leif on 2014-02-17.
 */
public class CommandLine {

    public static void main(String[] args){
        new CommandLine().read();
    }

    private boolean loop = true;
    private final Holder commandHolder;
    private CmdNode node;

    public CommandLine(){
        try {
            node = new CmdNode(4001);
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        commandHolder = new Holder(createCommands());
    }

    private Map<String, Command> createCommands(){
        Map<String, Command> commandMap = new HashMap<String, Command>();

        commandMap.put("exit", new Command() {
            @Override
            public void execute(List<String> args) {
                loop = false;
            }
        });

        commandMap.put("ignore", new Command() {
            @Override
            public void execute(List<String> args) {

            }
        });

        commandMap.put("start", new Command() {
            @Override
            public void execute(List<String> args) {

            }
        });

        commandMap.put("stop", new Command() {
            @Override
            public void execute(List<String> args) {
                node.shutdown();
            }
        });

        commandMap.put("put", new Command() {
            @Override
            public void execute(List<String> args) {
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
        });

        commandMap.put("get", new Command() {
            @Override
            public void execute(List<String> args) {
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
        });

        return commandMap;
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
}
