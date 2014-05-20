package se.chalmers.gdcn.ui;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
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
        Logger.getRootLogger().setLevel(Level.WARN);
        ClientInterface client = new PeerOwner();
        Console console = ConsoleFactory.create(client);
        console.read();
    }

    private final int HELP_INLINE = 24;

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

            @Override
            public WordInterface getWord() {
                return MetaCommand.EXIT;
            }
        });

        commandMap.put(MetaCommand.HELP.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                //TODO use TreeSet instead for alphabetical order?
                List<WordInterface> words = new ArrayList<>();
                words.addAll(Arrays.asList(CommandWord.values()));
                words.addAll(Arrays.asList(MetaCommand.values()));

                println("-- Commandname (arguments): description --");
                for(WordInterface word : words){
                    String init = word.getName() + " " + word.getArguments();
                    int whitespaces = HELP_INLINE - init.length();
                    println(init + StringUtils.repeat(" ", whitespaces) + word.getHelp());
                    println("");
                }
                println("");
            }

            @Override
            public WordInterface getWord() {
                return MetaCommand.HELP;
            }
        });

        commandMap.put(MetaCommand.ABOUT.getName(), new UICommand() {
            @Override
            public void execute(List<String> args) {
                println("GDCN - General Decentralized Computation Network\n");

                println("Developed in 2014 by\n" +
                        "\tJack Pettersson\n" +
                        "\tLeif Schelin\n" +
                        "\tNiklas Wärvik\n" +
                        "\tJoakim Öhman\n");

                println("@Chalmers University of Technology");
                println("Latest version can be found on\n" +
                        "\thttps://github.com/weeeeeew/GDCN");
            }

            @Override
            public WordInterface getWord() {
                return MetaCommand.ABOUT;
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
            case AUTO_WORK:
                println("Autowork on " + event.getOperation().getKey() + " " + success);
                break;
            case PUSH:
                println("Push job " + event.getOperation().getKey() + " " + success);
                break;
            case STOP:
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
            case INSTALL:
                println("Installation complete.");
                break;
            case UNINSTALL:
                println("Uninstallation complete.");
                break;
            default:
                println("Console: Returned cmd with unimplemented output: " + event.getCommandWord().getName());
                break;
        }
    }

    /**
     * Prints message + newline, exchangeable
     * @param message String to print
     */
    private void println(String message){
        System.out.println(message);
    }

    /**
     * Prints message, exchangeable
     * @param message String to print
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
                print("\n>> ");
                String line = bufferedReader.readLine();
                String[] words = line.split("\\s");
                List<String> wordList = new ArrayList<>(Arrays.asList(words));
                String cmd = wordList.remove(0);

                try{
                    commandHolder.execute(cmd, wordList);
                } catch (UnsupportedOperationException e){
                    println("Unsupported operation (\"" + cmd + "\").");
                    println("Type \"help\" to see a list of commands. Type \"exit\" to stop.");
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
