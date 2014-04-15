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
    private final Map<String, UICommand> commandMap = new HashMap<String, UICommand>();

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
        UICommand UICommand = commandMap.get(name);
        if(UICommand == null){
            //TODO if name exist in enum: throw NotImplementedException
            throw new UnsupportedOperationException("UICommand "+name+" doesn't exist!");
        }
        UICommand.execute(args);
    }
}
