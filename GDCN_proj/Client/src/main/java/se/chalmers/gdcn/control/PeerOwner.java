package se.chalmers.gdcn.control;

import net.tomp2p.futures.*;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.p2p.builder.BootstrapBuilder;
import net.tomp2p.p2p.builder.DiscoverBuilder;
import net.tomp2p.p2p.builder.GetBuilder;
import net.tomp2p.p2p.builder.PutBuilder;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.ErrorCode;
import se.chalmers.gdcn.communicationToUI.Operation.OperationBuilder;
import se.chalmers.gdcn.communicationToUI.OperationFinishedSupport;
import se.chalmers.gdcn.files.DataFilesManager;
import se.chalmers.gdcn.network.TaskPasser;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.Install;

import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.HashSet;
import java.util.List;


/**
 * Created by Leif on 2014-02-17
 */
public class PeerOwner implements se.chalmers.gdcn.communicationToUI.ClientInterface {

    //Peer implemented by TomP2P
    private Peer peer  = null;
    private TaskPasser taskPasser = null;
//<<<<<<< HEAD
//=======
//    private ReplicaManager replicaManager;
//>>>>>>> omrep

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
    private OperationFinishedSupport notifier = new OperationFinishedSupport(this);


    @Override
    public void addListener(PropertyChangeListener listener){
        notifier.addListener(listener);
    }

    @Override
    public void removeListener(PropertyChangeListener listener){
        notifier.removeListener(listener);
    }

//<<<<<<< HEAD
//=======
//    public PeerOwner() {
//
//        //TODO Make it possible to have a test replicaManager
//        dataFilesManager = new DataFilesManager();
//    }
//
//>>>>>>> omrep
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

    @Override
    public void work(String address, int port) {
        //TODO might want to continue on already downloaded task?

        if("self".equals(address)){
            taskPasser.requestWork(peer.getPeerAddress());
        } else {
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

                        taskPasser.requestWork(jobOwner);
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

    }

    @Override
    public void send(String msg) {
        //Sends to all known nodes, see what happens

        for(PeerAddress address : peer.getPeerBean().getPeerMap().getAll()){
            taskPasser.sendHello(address, msg);
        }
    }

    @Override
    public void put2(final String key, final String domain, Object value){
        PutBuilder builder = peer.put(Number160.createHash(key));
        try {
            builder.setData(Number160.createHash(domain), new Data(value));
            FutureDHT futureDHT = builder.start();
            futureDHT.addListener(new BaseFutureAdapter<BaseFuture>() {
                @Override
                public void operationComplete(BaseFuture future) throws Exception {
                    String s = future.isSuccess()?"succeeded":"failed";
                    System.out.println("Add under "+key+"/"+domain+s);
                }
            });
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    @Override
    public void get2(final String key, final String domain){
        GetBuilder getBuilder = peer.get(Number160.createHash(key));
        getBuilder.setContentKey(Number160.createHash(domain));
        FutureDHT futureDHT = getBuilder.start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {
            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                String s = future.isSuccess() ? "succeeded" : "failed";
                System.out.println("Get2 under " + key + "/" + domain + s);
                if (future.isSuccess()) {
                    System.out.println(future.getData().getObject().toString());
                }
            }
        });
    }


    @Override
    public void setNeighbourFile(String file){
        //dataFilesManager.changeNeighbourFileName(file);
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
        taskPasser.requestWork(getNeighbours().get(index%N));
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

//<<<<<<< HEAD
            taskPasser = new TaskPasser(peer, taskManager, this, dataFilesManager);
//=======
//            ReplicaManager replicaManager1 = dataFilesManager.getReplicaManager();
//
//            if(replicaManager1 == null) {
//                WorkerID myWorkerID = new WorkerID(peer.getPeerBean().getKeyPair().getPublic());
//                ReplicaManagerBuilder builder = new ReplicaManagerBuilder(myWorkerID, taskManager);
//                replicaManager = builder.create();
//            } else {
//                replicaManager1.setTaskManager(taskManager);
//                replicaManager = replicaManager1;
//            }
//
//            taskPasser = new TaskPasser(peer, replicaManager, taskManager, this, dataFilesManager);
//
//            timer = new Timer(true);
//            timer.schedule(new TimerTask() {
//                @Override
//                public void run() {
//                    dataFilesManager.saveReplicaManager(replicaManager);
//
//                    System.out.println("saving replicaManager");
//
//                }
//            }, 1000 * 120, 1000 * 120);
//>>>>>>> omrep


        } catch (NoSuchAlgorithmException | IOException e) {
            e.printStackTrace();
        }

        notifier.fireOperationFinished(CommandWord.START,
                new OperationBuilder<Integer>(peer != null).setResult(port).create());

    }
}
