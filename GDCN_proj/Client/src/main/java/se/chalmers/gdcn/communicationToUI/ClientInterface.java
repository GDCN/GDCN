package se.chalmers.gdcn.communicationToUI;

import se.chalmers.gdcn.Deceitful;

/**
 * Created by HalfLeif on 2014-02-26.
 */
public interface ClientInterface extends NetworkInterface{

    void install();
    void uninstall();

    void push(String jobName);

    void work(String address, int port, boolean autoWork);

    @Deceitful
    void falseWork(String address, int port);
}
