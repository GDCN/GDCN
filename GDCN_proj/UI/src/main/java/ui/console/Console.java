package ui.console;

import command.OLD_ClientLayer;
import command.communicationToUI.ClientInput;
import command.communicationToUI.ClientOutput;
import net.tomp2p.storage.Data;

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
    private final ClientOutput output = new ClientOutput() {

        //TODO implement these...
        @Override
        public void started(boolean success, int port, String errorMessage) {

        }

        @Override
        public void bootstrapped(boolean success, String host, int port, String errorMessage) {

        }

        @Override
        public void stopped(boolean success, String errorMessage) {

        }

        @Override
        public void put(boolean success, String name, Data data, String errorMessage) {

        }

        @Override
        public void got(boolean success, String name, Data data, String errorMessage) {

        }

        @Override
        public void gotNeighbors(boolean success, String errorMessage) {

        }

        @Override
        public void reBootstrapped(boolean success, String errorMessage) {

        }
    };

    private boolean loop = true;

    public Console(ClientInput client) {
        Map<String, Command> commandMap = ConsoleFactory.createCommands(client, this);
        this.commandHolder = new Holder(commandMap);
        client.addListener(this.output);
    }

    public static void main(String[] args){
        ClientInput client = new OLD_ClientLayer();
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
