package command.communicationToUI;

/**
 * Created by HalfLeif on 2014-02-26.
 */
public enum CommandWord {
    START(1, "start", "//help"),
    STOP(0, "stop", "//help"),
    PUT(2, "put", "//help"),
    GET(1, "get", "//help"),
    BOOTSTRAP(2, "bootstrap", "");

    private final int arity;
    private final String name;
    private final String help;

    private CommandWord(int arity, String name, String help){
        this.arity = arity;
        this.name = name;
        this.help = help;
    }

    public int getArity() {
        return arity;
    }

    public String getName() {
        return name;
    }

    public String getHelp() {
        return help;
    }
}
