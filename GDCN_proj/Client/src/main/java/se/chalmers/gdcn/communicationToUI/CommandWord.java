package se.chalmers.gdcn.communicationToUI;

/**
 * Created by HalfLeif on 2014-02-26.
 *
 * All supported commands by Client
 *
 * //TODO write help text
 */
public enum CommandWord implements WordInterface {
    START(1, "start", "//help"),
    STOP(0, "stop", "//help"),

    PUT(2, "put", "//help"),
    GET(1, "get", "//help"),

    BOOTSTRAP(2, "bootstrap", ""),

    WORK(2, "work", "//help"),
    AUTO_WORK(2, "autowork", "Works continually for the same job owner."),
    PUSH(1, "push", "Put job files to DHT"),

    INSTALL(0, "install", ""),
    UNINSTALL(0, "uninstall", ""),
    ;

    private final int arity;
    private final String name;
    private final String help;

    private CommandWord(int arity, String name, String help){
        this.arity = arity;
        this.name = name;
        this.help = help;
    }

    /**
     * Number of arguments this command can take
     * @return
     */
    @Override
    public int getArity() {
        return arity;
    }

    /**
     * Name of this command
     * @return
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * Help string for this command
     * @return
     */
    @Override
    public String getHelp() {
        return help;
    }
}
