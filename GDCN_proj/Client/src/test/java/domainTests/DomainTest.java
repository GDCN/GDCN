package domainTests;

import java.io.IOException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

import net.tomp2p.futures.FutureDHT;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.p2p.RequestP2PConfiguration;
import net.tomp2p.peers.Number160;
import net.tomp2p.storage.Data;
import net.tomp2p.storage.StorageGeneric;
import net.tomp2p.utils.Utils;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class DomainTest
{

    private Peer[] peers;
    Number160 peer2Owner;

    Peer peer1;
    Peer peer2;
    Peer peer3;
    Peer peer4;



    KeyPair pair1;
    KeyPair pair2;
    KeyPair pair3;
    KeyPair pair4;

    boolean success;

    @AfterMethod
    public void tearDown() {
        shutdown(peers);
    }

    @BeforeMethod
    public void setUp() throws IOException, NoSuchAlgorithmException {

        KeyPairGenerator gen = KeyPairGenerator.getInstance( "DSA" );

        pair1 = gen.generateKeyPair();
        pair2 = gen.generateKeyPair();
        pair3 = gen.generateKeyPair();
        pair4 = gen.generateKeyPair();

        peer2Owner = Utils.makeSHAHash( pair2.getPublic().getEncoded() );
        peer1 = new PeerMaker( pair1 ).setPorts(4001).makeAndListen();
        peer2 = new PeerMaker( pair2 ).setPorts(4002).makeAndListen();
        peer3 = new PeerMaker( pair3 ).setPorts(4003).makeAndListen();
        peer4 = new PeerMaker( pair4 ).setPorts(4004).makeAndListen();

        peers = new Peer[] { peer1, peer2, peer3, peer4 };
        ExampleUtils.bootstrap(peers);

        setProtection(peers, StorageGeneric.ProtectionEnable.NONE, StorageGeneric.ProtectionMode.NO_MASTER);
    }

    @Test
    public void allMaster()
            throws NoSuchAlgorithmException, IOException, ClassNotFoundException
    {

        setProtection( peers, StorageGeneric.ProtectionEnable.ALL , StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );
        // peer 1 stores "test" in the domain key of owner peer 2
        FutureDHT futurePut =
                peer1.put( Number160.ONE ).setData( new Data( "test" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();
        // peer 2 did not claim this domain, so we stored it
        Assert.assertTrue(futurePut.isSuccess());

        // peer 3 want to store something
        futurePut =
                peer3.put( Number160.ONE ).setData( new Data( "hello" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        Assert.assertFalse(futurePut.isSuccess());

        // peer 2 claims this domain
        futurePut =
                peer2.put( Number160.ONE ).setData( new Data( "MINE!" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        Assert.assertTrue(futurePut.isSuccess());

        // get the data!
        FutureDHT futureGet = peer1.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(futureGet.getData().getObject(), "MINE!");
    }

    @Test
    public void otherPeerMaster()
            throws NoSuchAlgorithmException, IOException, ClassNotFoundException
    {

        setProtection( peer3, StorageGeneric.ProtectionEnable.ALL , StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );
        // peer 1 stores "test" in the domain key of owner peer 2
        FutureDHT futurePut =
                peer3.put( Number160.ONE ).setData( new Data( "test" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();
        // peer 2 did not claim this domain, so we stored it
        Assert.assertTrue(futurePut.isSuccess());

        // peer 3 want to store something
        futurePut =
                peer1.put( Number160.ONE ).setData( new Data( "hello" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        Assert.assertTrue(futurePut.isSuccess());

        // peer 2 claims this domain
        futurePut =
                peer2.put( Number160.ONE ).setData( new Data( "MINE!" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        Assert.assertTrue(futurePut.isSuccess());

        futurePut =
                peer1.put( Number160.ONE ).setData( new Data( "hello" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        // get the data!
        FutureDHT futureGet = peer1.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(futureGet.getData().getObject(), "hello");
    }

    @Test
    public void allProtectionNoneMaster()
            throws NoSuchAlgorithmException, IOException, ClassNotFoundException
    {

        setProtection(peers, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.NO_MASTER);
        // peer 1 stores "test" in the domain key of owner peer 2
        FutureDHT futurePut =
                peer3.put( Number160.ONE ).setData( new Data( "test" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();
        // peer 2 did not claim this domain, so we stored it
        Assert.assertTrue(futurePut.isSuccess());

        // peer 3 want to store something
        futurePut =
                peer1.put( Number160.ONE ).setData( new Data( "hello" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        Assert.assertFalse(futurePut.isSuccess());

        // peer 2 claims this domain
        futurePut =
                peer2.put( Number160.ONE ).setData( new Data( "MINE!" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        Assert.assertFalse(futurePut.isSuccess());

        futurePut =
                peer3.put( Number160.ONE ).setData( new Data( "test2" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        // get the data!
        FutureDHT futureGet = peer1.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(futureGet.getData().getObject(), "test2");

        futurePut =
                peer3.put( Number160.ONE ).setData( new Data( "test3" ) ).setDomainKey( peer2Owner ).setProtectDomain().start();
        futurePut.awaitUninterruptibly();

        futureGet = peer2.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(futureGet.getData().getObject(), "test3");
    }

    @Test
    public void noneButOwnerMaster()
            throws NoSuchAlgorithmException, IOException, ClassNotFoundException
    {

        setProtection( peers, StorageGeneric.ProtectionEnable.NONE, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );
        // peer 1 stores "test" in the domain key of owner peer 2
        FutureDHT futurePut =
                peer1.put( Number160.ONE ).setData( new Data( "test" ) ).setProtectDomain().setDomainKey( peer2Owner ).start();
        futurePut.awaitUninterruptibly();
        // peer 2 did not claim this domain, so we stored it

        Assert.assertTrue(futurePut.isSuccess());

        // peer 3 want to store something
        futurePut =
                peer3.put( Number160.ONE ).setData( new Data( "hello" ) ).setProtectDomain().setDomainKey( peer2Owner ).start();
        futurePut.awaitUninterruptibly();

        Assert.assertTrue(futurePut.isSuccess());

        // peer 2 claims this domain
        futurePut =
                peer2.put( Number160.ONE ).setData( new Data( "MINE!" ) ).setProtectDomain().setDomainKey( peer2Owner ).start();
        futurePut.awaitUninterruptibly();

        Assert.assertTrue(futurePut.isSuccess());

        // get the data!
        FutureDHT futureGet = peer1.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(futureGet.getData().getObject(), "MINE!");

        futurePut = peer3.put( Number160.ONE ).setDomainKey( peer2Owner ).setData( new Data( "hello" ) ).start();
        futurePut.awaitUninterruptibly();

        Assert.assertFalse(futurePut.isSuccess());

    }

    @Test
    public void masterGoesDown()
            throws NoSuchAlgorithmException, IOException, ClassNotFoundException, InterruptedException {

        setProtection( peer2, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );
        setProtection( peer1, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );
        setProtection( peer4, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );

        // peer 1 stores "test" in the domain key of owner peer 2
        FutureDHT futurePut =
                peer1.put( Number160.ONE ).setData( new Data( "test" ) ).setProtectDomain().setDomainKey( peer2Owner ).start();
        futurePut.awaitUninterruptibly();
        // peer 2 did not claim this domain, so we stored it


        success = futurePut.isSuccess();

        Assert.assertTrue(success);

        // peer 3 want to store something
        futurePut =
                peer3.put( Number160.ONE ).setData( new Data( "hello" ) ).setProtectDomain().setDomainKey( peer2Owner ).start();
        futurePut.awaitUninterruptibly();

        success = futurePut.isSuccess();

        Assert.assertFalse(success);

        FutureDHT futureGet = peer1.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(futureGet.getData().getObject(), "test");


        peer2.shutdown();

        Thread.sleep(1000);


        futurePut = peer3.put( Number160.ONE ).setDomainKey( peer2Owner ).setData( new Data( "hello" ) ).start();
        futurePut.awaitUninterruptibly();

        success = futurePut.isSuccess();

        Assert.assertFalse(success);

        futureGet = peer1.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        futureGet = peer3.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Object object = futureGet.getData().getObject();

        System.out.println("Peer3 got: " + object + "\n");

        futureGet = peer2.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(object, "test");

    }

    @Test
    public void masterGoesDown2()
            throws NoSuchAlgorithmException, IOException, ClassNotFoundException, InterruptedException {

        setProtection( peer2, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );
        setProtection( peer1, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );
        setProtection( peer4, StorageGeneric.ProtectionEnable.NONE, StorageGeneric.ProtectionMode.NO_MASTER );


        KeyPairGenerator gen = KeyPairGenerator.getInstance( "DSA" );
        KeyPair pair5 = gen.generateKeyPair();
        KeyPair pair6 = gen.generateKeyPair();
        KeyPair pair7 = gen.generateKeyPair();
        KeyPair pair8 = gen.generateKeyPair();

        Peer peer5 = new PeerMaker( pair5 ).setPorts(4005).makeAndListen();
        Peer peer6 = new PeerMaker( pair6 ).setPorts(4006).makeAndListen();
        Peer peer7 = new PeerMaker( pair7 ).setPorts(4007).makeAndListen();
        Peer peer8 = new PeerMaker( pair8 ).setPorts(4008).makeAndListen();

        setProtection(peer5, StorageGeneric.ProtectionEnable.NONE, StorageGeneric.ProtectionMode.NO_MASTER);
        setProtection(peer3, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY);
        setProtection(peer7, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY);
        setProtection(peer8, StorageGeneric.ProtectionEnable.NONE, StorageGeneric.ProtectionMode.NO_MASTER);
        setProtection(peer6, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY);

        peers = new Peer[] {peer1,peer2,peer3,peer4,peer5, peer6, peer7};

        ExampleUtils.bootstrap(peers);

        // peer 1 stores "test" in the domain key of owner peer 2
        FutureDHT futurePut =
                peer1.put( Number160.ONE ).setData( new Data( "test" ) ).setProtectDomain().setDomainKey( peer2Owner ).start();
        futurePut.awaitUninterruptibly();
        // peer 2 did not claim this domain, so we stored it


        success = futurePut.isSuccess();

        Assert.assertTrue(success);

        // peer 3 want to store something
        futurePut =
                peer3.put( Number160.ONE ).setData( new Data( "hello" ) ).setProtectDomain().setDomainKey( peer2Owner ).start();
        futurePut.awaitUninterruptibly();

        success = futurePut.isSuccess();

        Assert.assertFalse(success);

        FutureDHT futureGet = peer1.get(Number160.ONE).setDomainKey(peer2Owner).setRequestP2PConfiguration(new RequestP2PConfiguration(8, 10, 0)).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(futureGet.getData().getObject(), "test");


        peer2.shutdown();

        Thread.sleep(1000);


        futurePut = peer3.put( Number160.ONE ).setDomainKey( peer2Owner ).setData( new Data( "hello" ) ).start();
        futurePut.awaitUninterruptibly();

        success = futurePut.isSuccess();

        Assert.assertFalse(success);

        futureGet = peer1.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

//        Assert.assertEquals(futureGet.getData().getObject(), "test");

        futureGet = peer3.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Object object = futureGet.getData().getObject();

        System.out.println("Peer3 got: " + object + "\n");

        Assert.assertEquals(object, "test");


    }

    @Test
    public void uploaderGoesDown()
            throws NoSuchAlgorithmException, IOException, ClassNotFoundException, InterruptedException {

        setProtection( peer2, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );
        setProtection( peer1, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );
        setProtection( peer4, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );

        // peer 1 stores "test" in the domain key of owner peer 2
        FutureDHT futurePut =
                peer1.put( Number160.ONE ).setData( new Data( "test" ) ).setProtectDomain().setDomainKey( peer2Owner ).start();
        futurePut.awaitUninterruptibly();
        // peer 2 did not claim this domain, so we stored it


        success = futurePut.isSuccess();

        Assert.assertTrue(success);

        // peer 3 want to store something
        futurePut =
                peer3.put( Number160.ONE ).setData( new Data( "hello" ) ).setProtectDomain().setDomainKey( peer2Owner ).start();
        futurePut.awaitUninterruptibly();

        success = futurePut.isSuccess();

        Assert.assertFalse(success);

        FutureDHT futureGet = peer1.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(futureGet.getData().getObject(), "test");


        peer1.shutdown();

        Thread.sleep(1000);


        futurePut = peer3.put( Number160.ONE ).setDomainKey( peer2Owner ).setData( new Data( "hello" ) ).start();
        futurePut.awaitUninterruptibly();

        success = futurePut.isSuccess();

        Assert.assertFalse(success);

        futureGet = peer2.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(futureGet.getData().getObject(), "test");

        futureGet = peer3.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(futureGet.getData().getObject(), "test");

    }

    @Test
    public void bothGoesDown()
            throws NoSuchAlgorithmException, IOException, ClassNotFoundException, InterruptedException {

        setProtection( peer2, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );
        setProtection( peer1, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );
        setProtection( peer4, StorageGeneric.ProtectionEnable.ALL, StorageGeneric.ProtectionMode.MASTER_PUBLIC_KEY );

        // peer 1 stores "test" in the domain key of owner peer 2
        FutureDHT futurePut =
                peer1.put( Number160.ONE ).setData( new Data( "test" ) ).setProtectDomain().setDomainKey( peer2Owner ).start();
        futurePut.awaitUninterruptibly();
        // peer 2 did not claim this domain, so we stored it


        success = futurePut.isSuccess();

        Assert.assertTrue(success);

        // peer 3 want to store something
        futurePut =
                peer3.put( Number160.ONE ).setData( new Data( "hello" ) ).setProtectDomain().setDomainKey( peer2Owner ).start();
        futurePut.awaitUninterruptibly();

        success = futurePut.isSuccess();

        Assert.assertFalse(success);

        FutureDHT futureGet = peer1.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Assert.assertEquals(futureGet.getData().getObject(), "test");


        peer1.shutdown();
        peer2.shutdown();

        Thread.sleep(1000);


        futurePut = peer3.put( Number160.ONE ).setDomainKey( peer2Owner ).setData( new Data( "hello" ) ).start();
        futurePut.awaitUninterruptibly();

        success = futurePut.isSuccess();

        Assert.assertFalse(success);

        futureGet = peer3.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        Object object = futureGet.getData().getObject();


        System.out.println("Peer3 got: " + object + "\n");

        peer2 = new PeerMaker( pair2 ).setPorts(4002).makeAndListen();

        Peer[] peers1 = new Peer[] {peer2, peer3};

        ExampleUtils.bootstrap(peers1);

        futureGet = peer2.get( Number160.ONE ).setDomainKey( peer2Owner ).start();
        futureGet.awaitUninterruptibly();

        object = futureGet.getData().getObject();

        System.out.println("Peer2 got: " + object + "\n");

        shutdown(peers1);

        Assert.assertEquals(object, "test");

    }

    private static void shutdown( Peer[] peers )
    {
        for ( Peer peer : peers )
        {
            if(!peer.isShutdown()) {
                peer.shutdown();
            }
        }
    }

    private static void setProtection( Peer[] peers, StorageGeneric.ProtectionEnable protectionEnable, StorageGeneric.ProtectionMode protectionMode )
    {
        for ( Peer peer : peers )
        {
            peer.getPeerBean().getStorage().setProtection(protectionEnable, protectionMode, protectionEnable,
                    protectionMode);
        }
    }

    private static void setProtection( Peer peer, StorageGeneric.ProtectionEnable protectionEnable, StorageGeneric.ProtectionMode protectionMode )
    {

        peer.getPeerBean().getStorage().setProtection(protectionEnable, protectionMode, protectionEnable,
                protectionMode);

    }
}