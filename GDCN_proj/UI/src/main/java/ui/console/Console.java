package ui.console;

import command.CommandLine;
import command.CommandLineImpl;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
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
        commandLine = new CommandLineImpl(4001);
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

            }
        });

        commandMap.put("stop", new Command() {
            @Override
            public void execute(List<String> args) {
                commandLine.stop(args);
            }
        });

        commandMap.put("discover2", new Command() {
            @Override
            public void execute(List<String> args) {
                commandLine.discover2(args);
            }
        });

        commandMap.put("discover", new Command() {
            @Override
            public void execute(List<String> args) {
                commandLine.discover(args);
            }
        });

        commandMap.put("put", new Command() {
            @Override
            public void execute(List<String> args) {
                commandLine.put(args);
            }
        });

        commandMap.put("get", new Command() {
            @Override
            public void execute(List<String> args) {
                commandLine.get(args);
            }
        });

        return commandMap;
    }
}
