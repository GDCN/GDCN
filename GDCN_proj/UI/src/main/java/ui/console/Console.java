package ui.console;

import command.ClientImplementation;
import command.ClientInput;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Created by HalfLeif on 2014-02-19.
 */
public class Console {

    private final Holder commandHolder;
    private final ClientInput client;

    private boolean loop = true;

    public Console(ClientInput client) {
        this.client = client;

        Map<String, Command> commandMap = ConsoleFactory.createCommands(client, this);
        commandHolder = new Holder(commandMap);
    }

    public static void main(String[] args){
        ClientInput client = new ClientImplementation();
        Console console = new Console(client);
        console.read();
    }

    /**
     * TODO Is it possible to this better?
     */
    void stop(){
        loop = false;
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
