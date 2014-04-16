package se.chalmers.gdcn.ui;

import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.OperationFinishedEvent;
import se.chalmers.gdcn.communicationToUI.WordInterface;
import se.chalmers.gdcn.control.PeerOwner;

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
 * Created by HalfLeif on 2014-02-19
 */
public class Console implements PropertyChangeListener{


    /**
     * Starts a client, adding a console to it and starts reading commands from CLI.
     * @param args
     */
    public static void main(String[] args){
        ClientInterface client = new PeerOwner();
        Console console = ConsoleFactory.create(client);
        console.read();
    }


    private final Holder commandHolder;

    private boolean loop = true;

    /**
     * Package-private constructor used by {@link se.chalmers.gdcn.ui.ConsoleFactory#create(se.chalmers.gdcn.communicationToUI.ClientInterface)}.
     * @param commandMap
     */
    Console(Map<String, UICommand> commandMap) {

        commandMap.put(MetaCommand.EXIT.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                loop = false;
            }
        });

        commandMap.put(MetaCommand.HELP.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                //TODO use TreeSet instead?
                List<WordInterface> words = new ArrayList<WordInterface>();
                words.addAll(Arrays.asList(CommandWord.values()));
                words.addAll(Arrays.asList(MetaCommand.values()));

                println("-- commandname (arity): description --");
                for(WordInterface word : words){
                    println(word.getName() + " (" + word.getArity() + "):\t" + word.getHelp());
                }
                println("");
            }
        });

        this.commandHolder = new Holder(commandMap);
    }

    /**
     * Receives results from the Client on various operations.
     * @param evt
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        OperationFinishedEvent event = (OperationFinishedEvent) evt;
        String success = event.getOperation().isSuccess()? "succeeded" : "failed";

        switch(event.getCommandWord()){
            case BOOTSTRAP:
                println("Bootstrap " + success);
                break;
            case WORK:
                println("Work on " + event.getOperation().getKey() + " " + success);
                break;
            case PUSH:
                println("Push job " + event.getOperation().getKey() + " " + success);
                break;
            case PUT:
                println("Put " + event.getOperation().getKey() + " " + success);
                break;
            case GET:
                println("Got " + event.getOperation().getKey() + " " + success);
                break;
            case START:
                println("Start complete.");
                break;
            default:
                println("Console: Returned cmd with unimplemented output: " + event.getCommandWord().getName());
                break;
        }
    }

    /**
     * Prints message + newline, exchangeable
     * @param message
     */
    private void println(String message){
        System.out.println(message);
    }

    /**
     * Prints message, exchangeable
     * @param message
     */
    private void print(String message){
        System.out.print(message);
    }

    /**
     * Loop for reading commands from CLI
     */
    public void read(){
        BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(System.in));
        try {

            while(loop){
                print(">> ");
                String line = bufferedReader.readLine();
                String[] words = line.split("\\s");
                List<String> wordList = new ArrayList<String>(Arrays.asList(words));
                String cmd = wordList.remove(0);

                try{
                    commandHolder.execute(cmd, wordList);
                } catch (UnsupportedOperationException e){
                    println("Unsupported operation (" + cmd + ").");
                    println("Type \"help\" to see a list of commands. Type \"exit\" to stop");
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