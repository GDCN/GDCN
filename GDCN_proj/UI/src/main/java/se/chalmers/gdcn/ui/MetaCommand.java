package se.chalmers.gdcn.ui;

import se.chalmers.gdcn.communicationToUI.WordInterface;

/**
 * Created by HalfLeif on 2014-02-26.
 *
 * Commands that only exist in context of Console, not in Client
 */
public enum MetaCommand implements WordInterface{
    ABOUT(0,"about","Text about this program."),
    HELP(0,"help", "This is the help command."),
    EXIT(0, "exit", "Exits the program.")
    ;

    private final int arity;
    private final String name;
    private final String help;

    MetaCommand(int arity, String name, String help) {
        this.arity = arity;
        this.name = name;
        this.help = help;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getArity() {
        return arity;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getArguments() {
        return "";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getHelp() {
        return help;
    }
}
