package ui.console;

import java.util.List;

/**
* Created by Leif on 2014-02-17.
*/
public interface Command {
    public void execute(List<String> args);
}
