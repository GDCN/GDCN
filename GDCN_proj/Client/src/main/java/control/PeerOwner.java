package control;

import command.communicationToUI.CommandWord;
import command.communicationToUI.ErrorCode;
import command.communicationToUI.Operation.OperationBuilder;
import command.communicationToUI.OperationFinishedSupport;
import net.tomp2p.futures.*;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.p2p.builder.BootstrapBuilder;
import net.tomp2p.p2p.builder.DiscoverBuilder;
import net.tomp2p.p2p.builder.GetBuilder;
import net.tomp2p.p2p.builder.PutBuilder;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.peers.PeerMapChangeListener;
import net.tomp2p.storage.Data;
import network.TaskPasser;
import taskbuilder.communicationToClient.TaskListener;
import taskbuilder.fileManagement.Install;

import java.beans.PropertyChangeListener;
import java.io.*;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by Leif on 2014-02-17
 */
public class PeerOwner implements command.communicationToUI.ClientInterface {

    //Peer implemented by TomP2P
    private Peer peer  = null;
    private TaskPasser taskPasser = null;

    private SettingsManager settingsManager;

    private NeighbourManager neighbourManager;

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

    private final TaskManager taskManager = new TaskManager(taskListener);

    private OperationFinishedSupport notifier = new OperationFinishedSupport(this);


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

        //Stops the peer if it is running to free the port
        if(peer != null) {
            stop();
        }

//        Good to use if testing multiple peers locally
//        fileName = fileName + port;

        neighbourManager = new NeighbourManager();
        settingsManager = new SettingsManager();

        try {

            //Initiates the peer
            KeyPairGenerator generator = KeyPairGenerator.getInstance("DSA");
            KeyPair keyPair = generator.generateKeyPair();

            peer = new PeerMaker( keyPair).setPorts(port).makeAndListen();

            //Reads the old neighbours which have been saved to file

            peer.getPeerBean().getPeerMap().addPeerMapChangeListener(neighbourManager.getPeerMapListener());

            taskPasser = new TaskPasser(peer);

        } catch (NoSuchAlgorithmException | IOException e) {
            e.printStackTrace();
        }

        notifier.fireOperationFinished(CommandWord.START,
                new OperationBuilder<Integer>(peer != null).setResult(port).create());
    }

    @Override
    public void stop(){
        //If it can't stop, Will not be any effects
        if(peer == null || peer.isShutdown()){
            notifier.fireOperationFinished(CommandWord.STOP,
                    new OperationBuilder(false).setErrorCode(ErrorCode.NOT_CONNECTED).create());

        } else {

            peer.shutdown();

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
    public void install() {
        Install.install();
    }

    @Override
    public void uninstall() {
        Install.uninstall();
    }

    @Override
    public void push(String jobName) {
        taskManager.uploadJob(jobName, this);
    }

    @Override
    public void work(String projectName, String taskName) {
        taskManager.startTask(projectName, taskName, this);
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

        HashSet<PeerAddress> fileNeighbours =  neighbourManager.getFileNeighbours();

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
                String s = future.isSuccess()?"succeeded":"failed";
                System.out.println("Get2 under "+key+"/"+domain+s);
                if(future.isSuccess()){
                    System.out.println(future.getData().getObject().toString());
                }
            }
        });
    }


    @Override
    public void setNeighbourFile(String file){
        neighbourManager.changeNeighbourFileName(file);
    }

    @Override
    public void clearNeighbourFile(){
        neighbourManager.clearNeighbourFile();
    }

    @Override
    public void deleteNeighbourFile(){
        neighbourManager.deleteNeighbourFile();
    }

    @Override
    public void requestWork(int index) {
        int N = getNeighbours().size();
        taskPasser.requestWork(getNeighbours().get(index%N));
    }

}