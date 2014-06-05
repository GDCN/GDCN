<<<<<<< HEAD
import control.PeerOwner;
import command.communicationToUI.CommandWord;
import command.communicationToUI.OperationFinishedEvent;
=======
import se.chalmers.gdcn.control.PeerOwner;
import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.OperationFinishedEvent;
>>>>>>> dev1
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.AfterTest;
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

    /**
     * Variables used by the tests
     */

    //Semaphoore used to make sure that everything is finished when doing things over
    //the netwok before proceeding.
    Semaphore sem;

    //Key used when getting and putting to the DHT
    String dhtKey = "Key";

    //The standard value used when putting to the DHT
    Data putValue;

    //The standard value which is saved to in getResultListener
    Data getValue;

    //The standard node used to rendevouz to.
    PeerOwner bootstrapNode;

    //The standard peer used by the tests
    PeerOwner peer;

    //The boolean used to see if the tests are successful.
    Boolean success;

    //Information about the bootstraping node to make it easier to bootstrap
    String bootstrapAdress = "localhost";
    int bootstrapPort = 4002;


    /**
     * Listeners used by the tests so that new ones do not have to be made for each test.
     * Each comand got its own listener. So if a test is doing multiple commands, multiple listeners is needed.
     */

    //only listens to the bootstrap command. releases the semaphore and sets success to true. Vital that
    //the semaphore is acquired after this and if the test is not about bootstraping that the
    //success of the test if checked some other way
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

    //See bootstraplistener
    PropertyChangeListener startListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {

            OperationFinishedEvent event = (OperationFinishedEvent) evt;

            if(event.getCommandWord() == CommandWord.START) {

                success = event.getOperation().isSuccess();
                sem.release();

            }
        }
    };

    //See bootstrapListener
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

    //Sets the success and the getValue so it can be confirmed in the test.
    //It also releases the semaphore
    PropertyChangeListener getListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {

            OperationFinishedEvent event = (OperationFinishedEvent) evt;

            if(event.getCommandWord() == CommandWord.GET) {

                success =  event.getOperation().isSuccess();
                getValue = (Data) event.getOperation().getResult();
                sem.release();
            }

        }
    };

    //See bootstrapListener
    PropertyChangeListener stopListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {

            OperationFinishedEvent event = (OperationFinishedEvent) evt;

            if(event.getCommandWord() == CommandWord.STOP) {

                success = event.getOperation().isSuccess();
                sem.release();

            }
        }
    };

    /**
     * Is run before each test.
     * resets the semaphore variable, the success variable and the standard nodes.
     * also sets the getValue to null and the putValue to the correct value.
     * @throws IOException
     */
    @BeforeMethod
    public void setUp() throws IOException {

        sem = new Semaphore(0);

        success = true;

        peer = new PeerOwner();
        bootstrapNode = new PeerOwner();

        peer.testStart(4001);
        bootstrapNode.testStart(bootstrapPort);

        putValue = new Data("value");
    }

    /**
     * Is run after each test and makes sure that the standard nodes is stopped.
     */
    @AfterMethod
    public void tearDown() {

        stopPeer(peer);
        stopPeer(bootstrapNode);

    }

    @AfterTest
    public void deleteTestDir() {
        peer.deleteTestDir();

    }


    /**
     * Tests if the bootstrap commands works by checking that the number of neighbours is equal to 1
     * and that the operation is a success.
     *
     */
    @Test
    public void bootStrapTest1() throws InterruptedException {

        peer.addListener(bootstrapListener);

        peer.bootstrap(bootstrapAdress, bootstrapPort);
        sem.acquire();

        Assert.assertEquals(peer.getNeighbours().size(), 1);
        Assert.assertTrue(success);

    }

    /**
     * Tests to make sure that the bootstrap command is not a success when the bootstrapNode is offline
     * and that the number of neighbours is zero.
     */
    @Test
    public void bootStrapTest2() throws InterruptedException {

        bootstrapNode.stop();

        peer.addListener(bootstrapListener);

        peer.bootstrap(bootstrapAdress, bootstrapPort);
        sem.acquire();

        Assert.assertFalse (success);
        Assert.assertEquals(peer.getNeighbours().size(), 0);

    }

    //Makes sure that a peer can bootstrap to a node twice without crashing
    //also makes sure that the number of neighbours is one.
    @Test
    public void bootStrapTest3() throws InterruptedException {

        peer.addListener(bootstrapListener);

        peer.bootstrap(bootstrapAdress, bootstrapPort);
        sem.acquire();

        Assert.assertTrue(success);

        peer.bootstrap(bootstrapAdress, bootstrapPort);
        sem.acquire();

        Assert.assertTrue(success);
        Assert.assertEquals(peer.getNeighbours().size(), 1);

    }

    //Makes sure that the number of neighbours is in the getNeighbours method.
    @Test
    public void getNeighbourTest1() throws InterruptedException {

        int numberOfPeers = 5;

        PeerOwner[] peers =  new PeerOwner[numberOfPeers];

        for(int i = 0; i < numberOfPeers; i++) {
            peers[i] = new PeerOwner();
            peers[i].addListener(bootstrapListener);
            peers[i].testStart(4003 + i);
        }

        for (int i = 0; i < numberOfPeers; i++) {
            peers[i].bootstrap(bootstrapAdress, bootstrapPort);
            sem.acquire();

            success = success && bootstrapNode.getNeighbours().size() == i+1;
            Assert.assertTrue(success);
        }

        List<PeerAddress> peerAddresses = bootstrapNode.getNeighbours();

        for(int i = 0; i < numberOfPeers; i++) {
            stopPeer(peers[i]);
        }

        Assert.assertTrue(success);
        Assert.assertEquals(numberOfPeers, peerAddresses.size());

    }


    /**
     * Makes sure that the put method works by putting a value and making sure that the operation is a success
     *
     */
    @Test
    public void putTest1() throws IOException, InterruptedException {

        peer.addListener(putListener);

        putValue = new Data("Value");

        peer.put(dhtKey, putValue);
        sem.acquire();

        Assert.assertTrue(success);
    }


    /**
     * Makes sure that the put overwrites a earlier value when the same key is used.
     *
     */
    @Test
    public void putTest2() throws IOException, InterruptedException, ClassNotFoundException {

        final Data putValue2 = new Data("secondValue");

        peer.addListener(getListener);
        peer.addListener(putListener);

        peer.put(dhtKey, putValue2);
        sem.acquire();

        Assert.assertTrue(success);

        peer.put(dhtKey, putValue);
        sem.acquire();

        Assert.assertTrue(success);

        peer.get(dhtKey);
        sem.acquire();

        Assert.assertEquals(getValue.getObject(), putValue.getObject());
        Assert.assertTrue(success);
    }


    /**
     * Tests get by bootstraping to peers to a bootstrap node, then one peer puts a value and the other one gets it
     * Is succesfull if the correct vailue is found.
     */
    @Test
    public void getTest1() throws IOException, InterruptedException, ClassNotFoundException {

        PeerOwner peer2 = new PeerOwner();
        putValue = new Data("Value");

        peer2.testStart(4003);

        peer2.addListener(putListener);
        peer.addListener(getListener);

        peer2.bootstrap(bootstrapAdress, bootstrapPort);
        peer.bootstrap(bootstrapAdress, bootstrapPort);

        peer2.put(dhtKey, putValue);
        sem.acquire();

        Assert.assertTrue(success);

        peer.get(dhtKey);
        sem.acquire();

        stopPeer(peer2);

        Assert.assertEquals(getValue.getObject(), putValue.getObject());
        Assert.assertTrue(success);

    }


    //Makes sure that the getValue is null when trying to get a nonexisting value.
    //Also makes sure that the success from the get method is false.
    @Test
    public void getTest2() throws InterruptedException {

        peer.addListener(getListener);
        peer.addListener(bootstrapListener);

        peer.bootstrap(bootstrapAdress, bootstrapPort);
        sem.acquire();

        Assert.assertTrue(success);

        peer.get("none-existing");
        sem.acquire();

        Assert.assertTrue(getValue == null);
        Assert.assertFalse(success);

    }

    //Makes sure that the start method works.
    @Test
    public void startTest1() throws InterruptedException {

        PeerOwner peer2 = new PeerOwner();

        peer2.addListener(startListener);

        peer2.testStart(4003);
        sem.acquire();

        stopPeer(peer2);

        Assert.assertTrue(success);

    }

    //Makes sure that the start method can be called when already running.
    @Test
    public void startTest2() throws InterruptedException {

        peer.addListener(startListener);

        peer.testStart(4003);
        sem.acquire();

        peer.deleteNeighbourFile();
        peer.deleteKeyFile();
        peer.deleteReplicaManager();

        Assert.assertTrue(success);

        peer.testStart(4001);
        sem.acquire();

        Assert.assertTrue(success);

    }

    //Makes sure that the stop method works.
    @Test
    public void stopTest1 () throws InterruptedException {

        peer.addListener(stopListener);

        peer.stop();
        sem.acquire();

        Assert.assertTrue(success);

    }

    //Makes sure that the stop method doesn't crash when called twice.
    @Test
    public void stopTest2 () throws InterruptedException {

        peer.addListener(stopListener);

        peer.stop();
        sem.acquire();

        Assert.assertTrue(success);

        peer.stop();
        sem.acquire();

        Assert.assertFalse(success);

    }

    //Makes sure that a nonrunning node can be stopped without crashing and that the success returned is false.
    @Test
    public void stopTest3 () throws InterruptedException {

        PeerOwner peer2 = new PeerOwner();

        peer2.addListener(stopListener);

        stopPeer(peer2);
        sem.acquire();

        Assert.assertFalse(success);

    }

    //Checks the rebootstrap method so that the number of reconnected nodes is correct and that
    //it is the same nodes.
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
            peers[i].testStart(4003 + i);
        }

        for (int i = 0; i < numberOfPeers; i++) {
            peers[i].bootstrap(bootstrapAdress, bootstrapPort);
        }

        sem.acquire(numberOfPeers);

        List<PeerAddress> peerAddressesBefore = bootstrapNode.getNeighbours();

        bootstrapNode.stop();
        sem.acquire();

        bootstrapNode.testStart(bootstrapPort);
        sem.acquire();

        bootstrapNode.reBootstrap();

        sem.acquire(numberOfPeers);


        List<PeerAddress> peerAddressesAfter = bootstrapNode.getNeighbours();

        for(int i = 0; i < numberOfPeers; i++) {
            stopPeer(peers[i]);
        }

        Assert.assertEquals(peerAddressesAfter.size(), numberOfPeers);
        Assert.assertEquals(peerAddressesAfter.size(), peerAddressesBefore.size());

        for(PeerAddress p : peerAddressesBefore) {
            Assert.assertTrue(peerAddressesAfter.contains(p));
        }

    }

    //Checks the rebootstrap method so that a node can reconect to nodes even
    //if not everyone is online.
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
            peers[i].testStart(4003 + i);
        }

        for (int i = 0; i < numberOfPeers; i++) {
            peers[i].bootstrap(bootstrapAdress, bootstrapPort);
        }

        for (int i = 0; i < numberOfPeersToStop; i++) {
            peers[i].addListener(stopListener);
        }

        sem.acquire(numberOfPeers);

        bootstrapNode.stop();

        for(int i = 0; i < numberOfPeersToStop; i++) {
            stopPeer(peers[i]);
        }

        sem.acquire(numberOfPeersToStop+1);

        bootstrapNode.testStart(bootstrapPort);
        sem.acquire();

        bootstrapNode.reBootstrap();
        sem.acquire(numberOfPeers);

        int numNeighbours = bootstrapNode.getNeighbours().size();

        for(int i = 0; i < numberOfPeers; i++) {
            stopPeer(peers[i]);
        }

        Assert.assertEquals(numNeighbours, numberOfPeers-numberOfPeersToStop);

    }

    public void stopPeer(PeerOwner p) {

        p.stop();

        p.deleteKeyFile();
        p.deleteNeighbourFile();
        p.deleteReplicaManager();

    }

//    Makes sure that the oldNeighbour list is updated
//    @Test
//    public void getOldNeighboursTest() throws InterruptedException {
//        int numberOfPeers = 5;
//
//        PeerOwner[] peers =  new PeerOwner[numberOfPeers];
//
//        for(int i = 0; i < numberOfPeers; i++) {
//            peers[i] = new PeerOwner();
//            peers[i].start(4003+i);
//            peers[i].bootstrap(bootstrapAdress, bootstrapPort);
//        }
//
//        while(bootstrapNode.getNeighbours().size() < 5) {
//            Thread.sleep(1000);
//        }
//
//        for(int i = 0; i < numberOfPeers; i++) {
//            peers[i].stop();
//        }
//
//        while (bootstrapNode.getOldNeighbours().size() < 5) {
//            Thread.sleep(1000);
//        }
//    }
}