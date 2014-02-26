import command.PeerOwner;
import command.communicationToUI.ClientInterface;
import net.tomp2p.storage.Data;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.testng.Assert;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.concurrent.Semaphore;

/**
 * Created by HalfLeif on 2014-02-25.
 */


//import org.testng.annotations.*;

public class ClientTest {

    Semaphore sem;

    String putKey = "Key";

    Data putValue;

    ClientInterface bootStrapNode;

    ClientInterface peer;

    Boolean success = false;



    @BeforeMethod
    public void setUp() throws IOException {
        sem = new Semaphore(0);

        peer = new PeerOwner();
        bootStrapNode = new PeerOwner();

        peer.start(4001);
        bootStrapNode.start(4002);


        putValue = new Data("Value");
    }


    @AfterMethod
    public void tearDown() {
        peer.stop();
        bootStrapNode.stop();

        success = false;
    }


    /**
     * Tests if the bootstrap commands works by checking that the number of neighbours is greater than one
     *
     */
    @Test
    public void bootStrapTest1() {

        PropertyChangeListener listener = new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                if(evt.getPropertyName() == "Bootstrap") {
                    success = (Boolean) evt.getOldValue();
                    sem.release();
                }
            }
        };

        peer.addListener(listener);

        peer.bootstrap("localhost", 4002);

        try {
            sem.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        assert (success);

        //assert (peer.getNeighbours().size() == 1);


    }

    @Test
    public void bootStrapTest2() {

        bootStrapNode.stop();

        PropertyChangeListener listener = new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                if(evt.getPropertyName() == "Bootstrap") {
                    success = (Boolean) evt.getOldValue();
                    sem.release();
                }
            }
        };

        peer.addListener(listener);

        peer.bootstrap("localhost", 4002);

        try {
            sem.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        Assert.assertFalse (success);

    }

    /**
     * Tests if the bootstrap commands works by checking that the number of neighbours is greater than one
     *
     */
    @Test
    public void putTest1() {

        PropertyChangeListener listener = new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                if(evt.getPropertyName() == "Put") {
                    success = (Boolean) evt.getOldValue();
                    sem.release();
                }
            }
        };

        peer.addListener(listener);


        peer.bootstrap("localhost", 4001);

        peer.put(putKey, putValue);

        try {
            sem.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        assert (success);
    }

    @Test
    public void getTest1() {

        ClientInterface peer2 = new PeerOwner();

        peer2.start(4003);

        PropertyChangeListener listener = new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                if(evt.getPropertyName() == "Get") {
                    success = (Boolean) evt.getOldValue();
                    sem.release();
                }
            }
        };

        peer.addListener(listener);

        peer2.bootstrap("localhost", 4002);
        peer.bootstrap("localhost", 4001);

        peer2.put(putKey, putValue);

        peer.get(putKey);


        try {
            sem.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        assert (success);

    }

    @Test
    public void getTest2() {

        ClientInterface peer2 = new PeerOwner();

        peer2.start(4003);

        PropertyChangeListener listener = new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                if(evt.getPropertyName() == "Get") {
                    success = (Boolean) evt.getOldValue();
                    sem.release();
                }
            }
        };

        peer.addListener(listener);

        peer2.bootstrap("localhost", 4002);
        peer.bootstrap("localhost", 4001);

        peer2.put(putKey, putValue);

        peer.get("none-existing");


        try {
            sem.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        Assert.assertFalse(success);

    }
}
