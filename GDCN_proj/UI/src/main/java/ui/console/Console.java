package ui.console;

import command.PeerOwner;
import command.communicationToUI.ClientInterface;
import command.communicationToUI.CommandWord;
import command.communicationToUI.OperationFinishedEvent;
import command.communicationToUI.WordInterface;

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

    private boolean loop = true;

    Console(Map<String, Command> commandMap) {

        commandMap.put(MetaCommand.EXIT.getName(), new Command() {
            @Override
            public void execute(List<String> args) {
                loop = false;
            }
        });

        commandMap.put(MetaCommand.HELP.getName(), new Command() {
            @Override
            public void execute(List<String> args) {
                List<WordInterface> words = new ArrayList<>();
                words.addAll(Arrays.asList(CommandWord.values()));
                words.addAll(Arrays.asList(MetaCommand.values()));

                print("-- commandname (arity): description --");
                for(WordInterface word : words){
                    print(word.getName()+" ("+word.getArity()+"):\t"+word.getHelp());
                }
            }
        });

        this.commandHolder = new Holder(commandMap);
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        OperationFinishedEvent event = (OperationFinishedEvent) evt;
        switch(event.getCommandWord()){
            case BOOTSTRAP:
                print("Bootstrap successful? " + event.getOperation().isSuccess());
                break;
            default:
                print("Console: Returned cmd with unimplemented output: " + event.getCommandWord().getName());
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
