package command.communicationToUI;

/**
 * Created by HalfLeif on 2014-02-26.
 */
public interface ClientInterface extends NetworkInterface{

    void install();
    void uninstall();

    void push(String jobName);

    void work(String projectName, String taskName);

    void put2(String key, String domain, Object value);

    void get2(String key, String domain);
}
