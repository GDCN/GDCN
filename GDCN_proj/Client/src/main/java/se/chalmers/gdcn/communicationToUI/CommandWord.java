package se.chalmers.gdcn.communicationToUI;

import java.io.File;

/**
 * Created by HalfLeif on 2014-02-26.
 *
 * All supported commands by Client
 *
 */
public enum CommandWord implements WordInterface {
    START(1, "start", "[port]", "Open [port] for network communication. If [port] is" + inline() +
            "left out, it will use the default port 4001."),
    STOP(0, "stop", "", "Close the port for network communication."),

    PUT(2, "put", "<key> <msg>", "Debug command: Put <msg> using <key>."),
    GET(1, "get", "<key>", "Debug command: Get the message of <key>."),

    BOOTSTRAP(2, "bootstrap", "[ip] [port]", "Connect to a network through the peer [ip] that is" + inline() +
            "using [port]. If the arguments are left out, it will" + inline() +
            "attempt to connect to the default bootstrap server."),

    WORK(2, "work", "<ip> <port>", "Work once for job owner with <ip> and <port>."),
    AUTO_WORK(2, "autowork", "<ip> <port>", "Work continually for job owner with <ip> and <port>."),
    PUSH(1, "push", "<job>", "Put <job> files to DHT. These files are found in" + inline() +
            "\"~"+ File.separator +".gdcn"+ File.separator +"jobs"+ File.separator +"<job>"+ File.separator+
            "\" or respective folder."),
    //TODO use Pathmanager to point to current directory?
    //PUSH(1, "push", "<job>", "Put <job> files to DHT. These are folders found in" + inline() +
    //        "the application folder's subfolder \"jobs\"."),

    INSTALL(0, "install", "", "Save initial application data and install libraries."),
    UNINSTALL(0, "uninstall", "", "Remove all application data."),
    ;

    // Note: Cannot use variable since that is illegal forward reference
    private static String inline() {
        // newline and 24 space characters
        return "\n                        ";
    }

    private final int arity;
    private final String name;
    private final String arguments;
    private final String help;

    private CommandWord(int arity, String name, String arguments, String help){
        this.arity = arity;
        this.name = name;
        this.arguments = arguments;
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
        return arguments;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getHelp() {
        return help;
    }
}