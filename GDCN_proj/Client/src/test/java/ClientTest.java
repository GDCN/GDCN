import command.PeerOwner;
import command.communicationToUI.ClientInterface;
import command.communicationToUI.CommandWord;
import command.communicationToUI.OperationFinishedEvent;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.testng.Assert;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.Semaphore;

/**
 * Created by HalfLeif on 2014-02-25
 */

public class ClientTest {

    //Variables used by most tests
    Semaphore sem;

    String putKey = "Key";

    Data putValue;

    PeerOwner bootstrapNode;

    PeerOwner peer;

    Boolean success = true;


    //Listeners used by the tests
    PropertyChangeListener bootstrapListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {

            OperationFinishedEvent event = (OperationFinishedEvent) evt;
            if(event.getCommandWord() == CommandWord.BOOTSTRAP) {
                success = event.getOperation().isSuccess();
                sem.release();
            }

        }
    };

    PropertyChangeListener startListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {

            OperationFinishedEvent event = (OperationFinishedEvent) evt;
            if(event.getCommandWord() == CommandWord.START) {
                success = success && event.getOperation().isSuccess();
                sem.release();

            }
        }
    };

    PropertyChangeListener putListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {

            OperationFinishedEvent event = (OperationFinishedEvent) evt;
            if(event.getCommandWord() == CommandWord.PUT) {
                success = event.getOperation().isSuccess();
                sem.release();
            }

        }
    };

    PropertyChangeListener getResultListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {

            OperationFinishedEvent event = (OperationFinishedEvent) evt;

            if(event.getCommandWord() == CommandWord.GET) {
                System.out.println(event.getOperation().isSuccess());
                Data result = (Data) event.getOperation().getResult();

                try {
                    success = success && event.getOperation().isSuccess()
                            && putValue.getObject().equals(result.getObject());
                } catch (ClassNotFoundException | IOException e) {
                    e.printStackTrace();
                }
                sem.release();
            }

        }
    };

    PropertyChangeListener getListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {

            OperationFinishedEvent event = (OperationFinishedEvent) evt;
            if(event.getCommandWord() == CommandWord.GET) {
                success = event.getOperation().isSuccess();
                sem.release();

            }
        }
    };

    PropertyChangeListener stopListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {

            OperationFinishedEvent event = (OperationFinishedEvent) evt;
            if(event.getCommandWord() == CommandWord.STOP) {
                success = success && event.getOperation().isSuccess();
                sem.release();

            }
        }
    };


    @BeforeMethod
    public void setUp() throws IOException {
        sem = new Semaphore(0);

        peer = new PeerOwner();
        bootstrapNode = new PeerOwner();

        peer.start(4001);
        bootstrapNode.start(4002);

        putValue = new Data("value");
    }


    @AfterMethod
    public void tearDown() {
        peer.stop();
        bootstrapNode.stop();

        success = true;
    }


    /**
     * Tests if the bootstrap commands works by checking that the number of neighbours is greater than one
     * and that the operation is a success.
     *
     */
    @Test
    public void bootStrapTest1() throws InterruptedException {

        peer.addListener(bootstrapListener);

        peer.bootstrap("localhost", 4002);
        sem.acquire();

        Assert.assertTrue (success && peer.getNeighbours().size() == 1);

    }

    /**
     * Tests to make sure that the bootstrap command is not a success when the bootstrapNode is offline
     * and that the number of neighbours is zero.
     *
     */
    @Test
    public void bootStrapTest2() throws InterruptedException {

        bootstrapNode.stop();

        peer.addListener(bootstrapListener);

        peer.bootstrap("localhost", 4002);
        sem.acquire();

        Assert.assertFalse (success);
        Assert.assertEquals(peer.getNeighbours().size(), 0);

    }

    @Test
    public void bootStrapTest3() throws InterruptedException {

        peer.addListener(bootstrapListener);

        peer.bootstrap("localhost", 4002);
        sem.acquire();

        peer.bootstrap("localhost", 4002);
        sem.acquire();

        Assert.assertTrue(success);
        Assert.assertEquals(peer.getNeighbours().size(), 1);

    }

    @Test
    public void getNeighbourTest() throws InterruptedException {

        int numberOfPeers = 5;

        PeerOwner[] peers =  new PeerOwner[numberOfPeers];

        for(int i = 0; i < numberOfPeers; i++) {
            peers[i] = new PeerOwner();
            peers[i].addListener(bootstrapListener);
            peers[i].start(4003+i);
        }

        for (int i = 0; i < numberOfPeers; i++) {
            peers[i].bootstrap("localhost", 4002);
            sem.acquire();
            success = success && bootstrapNode.getNeighbours().size() == i+1;
        }

        for(int i = 0; i < numberOfPeers; i++) {
            peers[i].stop();
        }

        Assert.assertTrue(success);
    }


    /**
     * Makes sure that the put method works by putting a value and making sure that the operation is a success
     *
     */
    @Test
    public void putTest1() throws IOException, InterruptedException {

        peer.addListener(putListener);

        putValue = new Data("Value");

        peer.put(putKey, putValue);
        sem.acquire();

        Assert.assertTrue(success);
    }


    /**
     * Makes sure that the put overwrites a earlier value when the same key is used.
     *
     */
    @Test
    public void putTest2() throws IOException, InterruptedException {

        final Data putValue2 = new Data("secondValue");

        success = true;

        peer.addListener(getResultListener);
        peer.addListener(putListener);

        peer.put(putKey, putValue2);
        sem.acquire();

        peer.put(putKey, putValue);
        sem.acquire();

        peer.get(putKey);
        sem.acquire();

        Assert.assertTrue(success);
    }


    /**
     * Tests get by bootstraping to peers to a bootstrap node, then one peer puts a value and the other one gets it
     *
     */
    @Test
    public void getTest1() throws IOException, InterruptedException {

        ClientInterface peer2 = new PeerOwner();
        putValue = new Data("Value");

        peer2.start(4003);

        peer2.addListener(putListener);
        peer.addListener(getListener);

        peer2.bootstrap("localhost", 4002);
        peer.bootstrap("localhost", 4002);

        peer2.put(putKey, putValue);
        sem.acquire();

        peer.get(putKey);
        sem.acquire();

        peer2.stop();

        Assert.assertTrue(success);

    }


    /**
     * Tests the get method by getting a value which not exists
     *
     */
    @Test
    public void getTest2() throws InterruptedException {

        peer.addListener(getListener);

        peer.bootstrap("localhost", 4002);

        peer.get("none-existing");
        sem.acquire();

        Assert.assertFalse(success);

    }

    /**
     * Tests start by starting the
     *
     */
    @Test
    public void startTest1() throws InterruptedException {
        ClientInterface peer2 = new PeerOwner();

        peer2.addListener(startListener);

        peer2.start(4003);
        sem.acquire();

        peer2.stop();

        Assert.assertTrue(success);

    }

    @Test
    public void startTest2() throws InterruptedException {

        success = true;

        peer.addListener(startListener);

        peer.start(4003);
        sem.acquire();

        peer.start(4001);
        sem.acquire();

        Assert.assertTrue(success);

    }

    @Test
    public void stopTest1 () throws InterruptedException {

        peer.addListener(stopListener);

        peer.stop();
        sem.acquire();

        Assert.assertTrue(success);

    }

    @Test
    public void stopTest2 () throws InterruptedException {

        peer.addListener(stopListener);

        peer.stop();
        sem.acquire();

        if(!success) {
            Assert.fail("first stop failed");
        }

        peer.stop();
        sem.acquire();

        Assert.assertFalse(success);

    }

    @Test
    public void stopTest3 () throws InterruptedException {

        ClientInterface peer2 = new PeerOwner();

        peer2.addListener(stopListener);

        peer2.stop();
        sem.acquire();

        Assert.assertFalse(success);

    }

    @Test
    public void rebootstrapTest1() throws InterruptedException {

        int numberOfPeers = 5;

        PeerOwner[] peers =  new PeerOwner[numberOfPeers];

        bootstrapNode.addListener(stopListener);
        bootstrapNode.addListener(startListener);
        bootstrapNode.addListener(bootstrapListener);

        for(int i = 0; i < numberOfPeers; i++) {
            peers[i] = new PeerOwner();
            peers[i].addListener(bootstrapListener);
            peers[i].start(4003+i);
        }

        for (int i = 0; i < numberOfPeers; i++) {
            peers[i].bootstrap("localhost", 4002);
        }

        sem.acquire(numberOfPeers);

        List<PeerAddress> peerAddressesBefore = bootstrapNode.getNeighbours();

        bootstrapNode.stop();
        sem.acquire();

        bootstrapNode.start(4002);
        sem.acquire();

        bootstrapNode.reBootstrap();
        sem.acquire(numberOfPeers);

        List<PeerAddress> peerAddressesAfter = bootstrapNode.getNeighbours();

        for(int i = 0; i < numberOfPeers; i++) {
            peers[i].stop();
        }

        Assert.assertEquals(peerAddressesAfter.size(), numberOfPeers);
        Assert.assertEquals(peerAddressesAfter.size(), peerAddressesBefore.size());
        for(PeerAddress p : peerAddressesBefore) {
            Assert.assertTrue(peerAddressesAfter.contains(p));
        }

    }

    @Test
    public void rebootstrapTest2() throws InterruptedException {

        int numberOfPeers = 5;

        int numberOfPeersToStop = 2;

        PeerOwner[] peers =  new PeerOwner[numberOfPeers];

        bootstrapNode.addListener(stopListener);
        bootstrapNode.addListener(startListener);
        bootstrapNode.addListener(bootstrapListener);

        for(int i = 0; i < numberOfPeers; i++) {
            peers[i] = new PeerOwner();
            peers[i].addListener(bootstrapListener);
            peers[i].start(4003+i);
        }

        for (int i = 0; i < numberOfPeers; i++) {
            peers[i].bootstrap("localhost", 4002);
        }

        for (int i = 0; i < numberOfPeersToStop; i++) {
            peers[i].addListener(stopListener);
        }

        sem.acquire(numberOfPeers);

        bootstrapNode.stop();

        for(int i = 0; i < numberOfPeersToStop; i++) {
            peers[i].stop();
        }

        sem.acquire(numberOfPeersToStop+1);

        bootstrapNode.start(4002);
        sem.acquire();

        bootstrapNode.reBootstrap();
        sem.acquire(numberOfPeers);

        int numNeighbours = bootstrapNode.getNeighbours().size();
        System.out.println("Number of oldNeighbours: " + bootstrapNode.getOldNeighbours().size());

        for(int i = 0; i < numberOfPeers; i++) {
            peers[i].stop();
        }

        Assert.assertEquals(numNeighbours, numberOfPeers-numberOfPeersToStop);

    }
}
