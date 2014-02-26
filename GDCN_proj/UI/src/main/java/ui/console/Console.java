package ui.console;

import command.PeerOwner;
import command.communicationToUI.ClientInterface;
import command.communicationToUI.OperationFinishedEvent;

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
public class Console implements PropertyChangeListener{

    private final Holder commandHolder;

    Console(Map<String, Command> commandMap) {
        this.commandHolder = new Holder(commandMap);
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        OperationFinishedEvent event = (OperationFinishedEvent) evt;
        switch(event.getOldValue()){
            case BOOTSTRAP:
                print("Bootstrap: " + evt.getNewValue());
                break;
            default:
                print("Console: Returned cmd with unimplemented output: " + event.getOldValue().getName());
                break;
        }
    }

    private void print(String message){
        System.out.println(message);
    }

    public static void main(String[] args){
        ClientInterface client = new PeerOwner();
        Console console = ConsoleFactory.create(client);
        console.read();
    }

    public void read(){
        BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(System.in));
        boolean loop = true;
        try {

            while(loop){
                System.out.print(">> ");
                String line = bufferedReader.readLine();
                String[] words = line.split("\\s");
                List<String> wordList = new ArrayList<String>(Arrays.asList(words));
                String cmd = wordList.remove(0);

                if("exit".equals(cmd)){
                    loop = false;
                    commandHolder.execute("stop", wordList);
                } else {
                    try{
                        commandHolder.execute(cmd, wordList);
                    } catch (UnsupportedOperationException e){
                        System.out.println("Unsupported operation ("+cmd+"). Type \"exit\" to stop");
                    }
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
