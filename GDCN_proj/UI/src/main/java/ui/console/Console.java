package ui.console;

import command.PeerOwner;
import command.communicationToUI.ClientInterface;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
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
    private final PropertyChangeListener listener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            switch(evt.getPropertyName()){
                case "Bootstrap":
                    print("Bootstrap: "+evt.getOldValue());
                    break;
                default:
                    print("Console: Returned unimplemented name: "+evt.getPropertyName());
                    break;
            }
        }
    };

    private void print(String message){
        System.out.println(message);
    }

    private boolean loop = true;

    public Console(ClientInterface client) {
        Map<String, Command> commandMap = ConsoleFactory.createCommands(client, this);
        this.commandHolder = new Holder(commandMap);
        client.addListener(this.listener);
    }

    public static void main(String[] args){
        ClientInterface client = new PeerOwner();
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
