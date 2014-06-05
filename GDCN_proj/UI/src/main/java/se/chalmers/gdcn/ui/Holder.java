package se.chalmers.gdcn.ui;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
* Created by Leif on 2014-02-17.
 *
 * Immutable class to hold commands for Console
*/
public class Holder {
    private final Map<String, UICommand> commandMap = new HashMap<>();

    public Holder(Map<String, UICommand> commandMap){
        this.commandMap.putAll(commandMap);
    }

    /**
     * Attempts to execute command with provided name.
     * @param name name of command
     * @param args arguments to the command
     * @throws UnsupportedOperationException if name doesn't exist
     */
    public void execute(String name, List<String> args) throws UnsupportedOperationException{
        UICommand uiCommand = commandMap.get(name);
        if(uiCommand == null){
            throw new UnsupportedOperationException("UICommand "+name+" wasn't found!");
        }
        int arity = uiCommand.getWord().getArity();
        if(arity < args.size()){
            System.out.println("Warning: argument ignored.");
            if(arity==0){
                System.out.println("'"+uiCommand.getWord().getName()+"' takes no arguments.\n");
            } else {
                System.out.println("'"+uiCommand.getWord().getName()+"' only takes "+arity+" arguments.\n");
            }
        }
        uiCommand.execute(args);
    }
}
