package se.chalmers.gdcn.control;

import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureBootstrap;
import net.tomp2p.futures.FutureDHT;
import net.tomp2p.futures.FutureDiscover;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.p2p.builder.BootstrapBuilder;
import net.tomp2p.p2p.builder.DiscoverBuilder;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.peers.PeerMapChangeListener;
import net.tomp2p.storage.Data;
import se.chalmers.gdcn.deceitful.*;
import se.chalmers.gdcn.communicationToUI.*;
import se.chalmers.gdcn.communicationToUI.Operation.OperationBuilder;
import se.chalmers.gdcn.files.DataFilesManager;
import se.chalmers.gdcn.network.TaskPasser;
import se.chalmers.gdcn.replica.ReplicaManager;
import se.chalmers.gdcn.replica.ReplicaManager.ReplicaID;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.Install;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.HashSet;
import java.util.List;
import java.util.Map;


/**
 * Created by Leif on 2014-02-17
 */
public class PeerOwner implements se.chalmers.gdcn.communicationToUI.ClientInterface {

    //Peer implemented by TomP2P
    private Peer peer  = null;
    private TaskPasser taskPasser = null;

    private DataFilesManager dataFilesManager;

    private String testPath = "TEST";

    //Listener used by UI to react to results from commands
    private final TaskListener taskListener = new TaskListener() {
        @Override
        public void taskFinished(String taskName) {
            notifier.fireOperationFinished(CommandWord.WORK, new OperationBuilder<String>(true).setKey(taskName).create());
        }

        @Override
        public void taskFailed(String taskName, String reason) {
            notifier.fireOperationFinished(CommandWord.WORK, new OperationBuilder<String>(false).setKey(taskName).create());
        }
    };

    private final TaskManager taskManager = new TaskManager(taskListener, this);
    private final OperationFinishedSupport notifier = new OperationFinishedSupport(this);


    @Override
    public void addListener(PropertyChangeListener listener){
        notifier.addListener(listener);
    }

    @Override
    public void removeListener(PropertyChangeListener listener){
        notifier.removeListener(listener);
    }

    @Override
    public void start(int port){

        dataFilesManager = new DataFilesManager();

        startInitiate(port);
    }

    /**
     * Only used by tests.
     *
     *
     */
    public void testStart(int port){


        dataFilesManager = new DataFilesManager(testPath, port + "");

        startInitiate(port);

    }

    @Override
    public void stop(){
        //If it can't stop, Will not be any effects
        if(peer == null || peer.isShutdown()){
            notifier.fireOperationFinished(CommandWord.STOP,
                    new OperationBuilder(false).setErrorCode(ErrorCode.NOT_CONNECTED).create());

        } else {

            peer.shutdown();

            taskPasser.stopTimer();

            notifier.fireOperationFinished(CommandWord.STOP, new OperationBuilder(true).create());

        }
    }

    @Override
    public void bootstrap() {
        List<String[]> bsn = dataFilesManager.getBootstrapNodes();

        if(bsn.size() == 0){
            System.out.println("No known nodes to bootstrap to.");
            //TODO attempt default bootstrap!
        }

        for(String[] s : bsn) {
            bootstrap(s[0], Integer.parseInt(s[1]));

            System.out.println("Attempt bootstrap to "+s[0]+" : "+s[1]);
            if(getNeighbours().size() > 0) {
                return;
            }
        }
    }

    @Override
    public void bootstrap(final String host, final int port){
        try {
            final InetAddress inetAddress = InetAddress.getByName(host);

            DiscoverBuilder discoverBuilder = peer.discover().setInetAddress(inetAddress).setPorts(port);
            FutureDiscover futureDiscover = discoverBuilder.start();


            futureDiscover.addListener(new BaseFutureAdapter<FutureDiscover>() {
                @Override
                public void operationComplete(FutureDiscover future) throws Exception {
                    if(!future.isSuccess()){
                        notifier.fireOperationFinished(CommandWord.BOOTSTRAP,
                                new OperationBuilder<InetAddress>(false).setErrorCode(ErrorCode.DISCOVER_FAILURE).create());
                        return;
                    }

                    BootstrapBuilder bootstrapBuilder = peer.bootstrap().setInetAddress(inetAddress).setPorts(port);
                    FutureBootstrap futureBootstrap = bootstrapBuilder.start();

                    futureBootstrap.addListener(new BaseFutureAdapter<FutureBootstrap>() {
                        @Override
                        public void operationComplete(FutureBootstrap future) throws Exception {
                            if(!future.isSuccess()){
                                notifier.fireOperationFinished(CommandWord.BOOTSTRAP,
                                        new OperationBuilder<InetAddress>(false).setResult(inetAddress).setErrorCode(ErrorCode.BOOTSTRAP_FAILURE).create());
                                return;
                            }
                            notifier.fireOperationFinished(CommandWord.BOOTSTRAP,
                                    new OperationBuilder<InetAddress>(true).setResult(inetAddress).create());
                        }
                    });
                }
            });
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }

    }

    @Override
    public void put(final String name, final Data value){
        //TODO this method might not be used by TaskPasser at all when uploading files...
        //TODO Remove entirely? Good for debug and is used by JobUploader.
        FutureDHT futureDHT = peer.put(Number160.createHash(name)).setData(value).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {

            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                boolean success = future.isSuccess();

                notifier.fireOperationFinished(CommandWord.PUT, new OperationBuilder<Data>(success).setResult(value).setKey(name).create());
            }
        });
    }

    @Override
    public void put(final Number160 key, final Data value) {
        FutureDHT futureDHT = peer.put(key).setData(value).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {

            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                boolean success = future.isSuccess();

                notifier.fireOperationFinished(CommandWord.PUT, new OperationBuilder<Data>(success).setResult(value).setKey(key).create());
            }
        });
    }

    @Override
    public void put(final Number160 key, final Number160 domain, final Data value){
        FutureDHT futureDHT = peer.put(key).setData(value).setDomainKey(domain).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {

            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                boolean success = future.isSuccess();

                notifier.fireOperationFinished(CommandWord.PUT, new OperationBuilder<Data>(success).setResult(value).setKey(key).create());
            }
        });
    }

    @Override
    public void get(final String name){
        FutureDHT futureDHT = peer.get(Number160.createHash(name)).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {
            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                boolean success = future.isSuccess();
                notifier.fireOperationFinished(CommandWord.GET,
                        new OperationBuilder<Data>(success).setKey(name).setResult(future.getData()).create());
            }
        });

    }

    @Override
    public void get(final Number160 key) {
        FutureDHT futureDHT = peer.get(key).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {
            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                boolean success = future.isSuccess();
                notifier.fireOperationFinished(CommandWord.GET,
                        new OperationBuilder<Data>(success).setKey(key).setResult(future.getData()).create());
            }
        });
    }

    @Override
    public void install() {
        Install.install();
    }

    @Override
    public void uninstall() {
        Install.uninstall();
    }

    @Override
    public void push(String jobName) {
        //TODO Move to other class?
        taskManager.uploadJob(jobName, taskPasser.getReplicaManager());
    }

    @Deceitful
    @Override
    public void falseWork(String address, int port){
        deceit(address, port, new FalseWork(taskPasser, this, peer));
    }

    @Deceitful
    @Override
    public void spamWork(String address, int port){
        deceit(address, port, new SpamWork(1,taskManager, this));
    }

    @Deceitful
    @Override
    public void stopWork(String address, int port){
        deceit(address, port, new StopWork(this, taskPasser, peer));
    }

    @Deceitful
    private void deceit(String address, int port, final DeceitfulWork deceitfulWork){
        if("self".equals(address)){
            deceitfulWork.requestWork(peer.getPeerAddress());
            return;
        }

        try {
            DiscoverBuilder discoverBuilder = peer.discover().setInetAddress(InetAddress.getByName(address)).setPorts(port);
            discoverBuilder.start().addListener(new BaseFutureAdapter<FutureDiscover>(){
                @Override
                public void operationComplete(FutureDiscover future) throws Exception {
                    if(!future.isSuccess()){
                        notifier.fireOperationFinished(CommandWord.WORK,
                                new OperationBuilder<>(false).setErrorCode(ErrorCode.DISCOVER_FAILURE).create());
                        return;
                    }
                    PeerAddress jobOwner = future.getReporter();
                    assert ! jobOwner.equals(peer.getPeerAddress());

                    deceitfulWork.requestWork(jobOwner);
                }
            });
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void work(String address, int port, final boolean autoWork) {
        //TODO might want to continue on already downloaded task?

        if("self".equals(address)){
            taskPasser.requestWork(peer.getPeerAddress(), autoWork);
        } else {
            try {
                System.out.println("Attempt work for "+address+" : "+port);
                DiscoverBuilder discoverBuilder = peer.discover().setInetAddress(InetAddress.getByName(address)).setPorts(port);
                discoverBuilder.start().addListener(new BaseFutureAdapter<FutureDiscover>(){
                    @Override
                    public void operationComplete(FutureDiscover future) throws Exception {
                        if(!future.isSuccess()){
                            notifier.fireOperationFinished(CommandWord.WORK,
                                    new OperationBuilder<>(false).setErrorCode(ErrorCode.DISCOVER_FAILURE)
                                            .setReason(future.getFailedReason()).create());
                            return;
                        }
                        PeerAddress jobOwner = future.getReporter();
                        assert ! jobOwner.equals(peer.getPeerAddress());

                        taskPasser.requestWork(jobOwner, autoWork);
                    }
                });
            } catch (UnknownHostException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    public List<PeerAddress> getNeighbours(){

        return peer.getPeerBean().getPeerMap().getAll();
    }

    @Override
    public List<PeerAddress> getOldNeighbours() {
        return null;
    }

    @Override
    public void reBootstrap() {

        HashSet<PeerAddress> fileNeighbours =  dataFilesManager.getFileNeighbours();

        for(PeerAddress p : fileNeighbours) {
            bootstrap(p.getInetAddress().getHostAddress(),p.portTCP());
        }
        if(!enoughNeighbours()) {
            bootstrap();
        }

    }

    private boolean enoughNeighbours() {
        return peer.getPeerBean().getPeerMap().getAll().size() > 0;
    }

    @Override
    public void send(String msg) {
        //Sends to all known nodes, see what happens

        for(PeerAddress address : peer.getPeerBean().getPeerMap().getAll()){
            taskPasser.sendHello(address, msg);
        }
    }

    @Override
    public void get(final Number160 key, final Number160 domain){
        FutureDHT futureDHT = peer.get(key).setDomainKey(domain).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {
            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                boolean success = future.isSuccess();
                notifier.fireOperationFinished(CommandWord.GET,
                        new OperationBuilder<Data>(success).setKey(key).setResult(future.getData()).create());
            }
        });
    }


    @Override
    public void setNeighbourFile(String file){
//        dataFilesManager.changeNeighbourFileName(file);
    }

    @Override
    public void clearNeighbourFile(){
        //neighbourFileManager.clearNeighbourFile();
    }

    @Override
    public void deleteNeighbourFile(){
        if(dataFilesManager != null) {
            dataFilesManager.removeNeighbourFile();
        }
    }

    public void deleteKeyFile() {
        if(dataFilesManager != null) {
            dataFilesManager.removeKeyFile();
        }
    }

    public void deleteReplicaManager() {
        if(dataFilesManager != null) {
            dataFilesManager.removeReplicaManagerFile();
        }
    }

    public void deleteTestDir() {
        if(dataFilesManager != null) {
            dataFilesManager.deleteTestDir();
        }
    }

    @Override
    public void requestWork(int index) {
        int N = getNeighbours().size();
        taskPasser.requestWork(getNeighbours().get(index % N), false);
    }

    private void startInitiate(int port) {

        //Stops the peer if it is running to free the port
        if(peer != null) {
            stop();
        }

        try {

            //Initiates the peer
            KeyPair keyPair = dataFilesManager.getKeypair();

            if(keyPair == null) {
                KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA");
                keyPair = generator.generateKeyPair();
                dataFilesManager.saveKeyPair(keyPair);
            }

            peer = new PeerMaker( keyPair).setPorts(port).makeAndListen();
            peer.getPeerBean().getPeerMap().addPeerMapChangeListener(dataFilesManager.getPeerMapListener());

            taskPasser = new TaskPasser(peer, taskManager, this, dataFilesManager);

        } catch (NoSuchAlgorithmException | IOException e) {
            e.printStackTrace();
        }

        String myAddress = null;
        if(peer!= null){
            try {
                //peer.getPeerAddress().getInetAddress().getHostAddress() returns 127.0.0.1 instead of local IP
                //might be useful to return global IP
                myAddress = InetAddress.getLocalHost().getHostAddress()+" : "+port;
            } catch (UnknownHostException e) {
                e.printStackTrace();
                //ignore
            }
        }
        notifier.fireOperationFinished(CommandWord.START,
                new OperationBuilder<String>(peer != null).setResult(myAddress)
                        .setReason("Peer could not be created for some reason!").create());

//        downloadEventualResults();
        peer.getPeerBean().getPeerMap().addPeerMapChangeListener(new PeerMapChangeListener() {
            @Override
            public void peerInserted(PeerAddress peerAddress) {
                if(getNeighbours().size()<2){
                    downloadEventualResults();
                }
            }

            @Override
            public void peerRemoved(PeerAddress peerAddress) {
                //ignore
            }

            @Override
            public void peerUpdated(PeerAddress peerAddress) {
                //ignore
            }
        });
    }

    /**
     * Attempts to download results to previously given keys.
     * Need to be connected to the DHT in order for this to be useful
     */
    private void downloadEventualResults(){
        final ReplicaManager replicaManager = taskPasser.getReplicaManager();
        Map<ReplicaID,Number160> pendingResults = replicaManager.pendingResults();
        for(final ReplicaID replicaID : pendingResults.keySet()){
            System.out.println("See if "+replicaID+" has uploaded something");

            Number160 key = pendingResults.get(replicaID);
            addListener(new OperationFinishedListener(this, key, CommandWord.PUT) {
                @Override
                protected void operationFinished(Operation operation) {
                    if(operation.isSuccess()){
                        Data result = (Data) operation.getResult();
                        try {
                            byte[] resultObject = (byte[]) result.getObject();
                            System.out.println("Result downloaded for "+replicaID+" on "+resultObject.length+" bytes.");
                            replicaManager.replicaFinished(replicaID, resultObject);
                        } catch (ClassNotFoundException e) {
                            e.printStackTrace();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                }
            });
            this.get(key);
        }
    }

    @Override
    public Number160 getID() {
        return peer.getPeerID();
    }
}
