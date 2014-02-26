package ui.console;

import command.communicationToUI.WordInterface;

/**
 * Created by HalfLeif on 2014-02-26.
 */
public enum MetaCommand implements WordInterface{
    HELP(0,"help","This is the help command"),
    EXIT(0, "exit", "Exits the program")
    ;

    private final int arity;
    private final String name;
    private final String help;

    MetaCommand(int arity, String name, String help) {
        this.arity = arity;
        this.name = name;
        this.help = help;
    }

    @Override
    public int getArity() {
        return arity;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getHelp() {
        return help;
    }
}
