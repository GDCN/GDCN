package ui.console;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
* Created by Leif on 2014-02-17.
 *
 * Immutable class to hold commands for Console
*/
public class Holder {
    private final Map<String, Command> commandMap = new HashMap<String, Command>();

    public Holder(Map<String, Command> commandMap){
        this.commandMap.putAll(commandMap);
    }

    /**
     * Attempts to execute command with provided name.
     * @param name name of command
     * @param args arguments to the command
     * @throws UnsupportedOperationException if name doesn't exist
     */
    public void execute(String name, List<String> args) throws UnsupportedOperationException{
        Command command = commandMap.get(name);
        if(command == null){
            //TODO if name exist in enum: throw NotImplementedException
            throw new UnsupportedOperationException("Command "+name+" doesn't exist!");
        }
        command.execute(args);
    }
}
