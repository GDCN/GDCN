package ui.console;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
* Created by Leif on 2014-02-17.
*/
public class Holder {
    private final Map<String, Command> commandMap = new HashMap<String, Command>();

    public Holder(Map<String, Command> commandMap){
        this.commandMap.putAll(commandMap);
    }

    public void execute(String name, List<String> args) throws UnsupportedOperationException{
        Command command = commandMap.get(name);
        if(command == null){
            throw new UnsupportedOperationException("Command "+name+" is not supported!");
        }
        command.execute(args);
    }
}
