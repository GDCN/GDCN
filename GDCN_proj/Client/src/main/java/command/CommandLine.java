package command;

import java.util.List;

/**
 * Created by HalfLeif on 2014-02-19.
 */
public interface CommandLine {
    public boolean isConnected();

    public void stop(List<String> args);

    public void discover2(List<String> args);

    public void discover(List<String> args);

    public void put(List<String> args);

    public void get(List<String> args);
}
