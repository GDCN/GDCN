package control;

import command.communicationToUI.CommandWord;
import command.communicationToUI.ErrorCode;
import command.communicationToUI.Operation.OperationBuilder;
import command.communicationToUI.OperationFinishedSupport;
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
import taskbuilder.communicationToClient.TaskListener;
import control.TaskManager;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.*;

/**
 * Created by Leif on 2014-02-17
 */
public class PeerOwner implements command.communicationToUI.ClientInterface {

    private Peer peer  = null;
    private List<PeerAddress> neighbours = new ArrayList<>();
    private List<PeerAddress> oldNeighbours = new ArrayList<>();


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

    private final PeerMapChangeListener peerMapChangeListener = new PeerMapChangeListener() {
        @Override
        public void peerInserted(PeerAddress peerAddress) {

            //TODO save node to file here?

            if(oldNeighbours.contains(peerAddress)) {

                oldNeighbours.remove(peerAddress);
            }

            if(!neighbours.contains(peerAddress)) {
                neighbours.add(peerAddress);
            }
        }

        @Override
        public void peerRemoved(PeerAddress peerAddress) {

            //TODO Change the nodes saved locally here?

            neighbours.remove(peerAddress);

            oldNeighbours.add(peerAddress);
        }

        @Override
        public void peerUpdated(PeerAddress peerAddress) {
            //TODO save node to file here?

            neighbours.remove(peerAddress);
            neighbours.add(peerAddress);

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

        if(peer != null) {
            stop();
        }

        try {

            KeyPairGenerator generator = KeyPairGenerator.getInstance("DSA");
            KeyPair keyPair = generator.generateKeyPair();
            peer = new PeerMaker( keyPair).setPorts(port).makeAndListen();

            peer.getPeerBean().getPeerMap().addPeerMapChangeListener(peerMapChangeListener);

        } catch (NoSuchAlgorithmException | IOException e) {
            e.printStackTrace();
        }

        notifier.fireOperationFinished(CommandWord.START,
                new OperationBuilder<Integer>(peer != null).setResult(port).create());
    }

    @Override
    public void stop(){
        if(peer == null || peer.isShutdown()){
            notifier.fireOperationFinished(CommandWord.STOP,
                    new OperationBuilder(false).setErrorCode(ErrorCode.NOT_CONNECTED).create());
            return;
        }
        if(!peer.isShutdown()){
            peer.shutdown();
        }
        notifier.fireOperationFinished(CommandWord.STOP, new OperationBuilder(true).create());
    }

    @Override
    public void bootstrap(final String host, final int port){
        try {
            final InetAddress inetAddress = InetAddress.getByName(host);

            DiscoverBuilder discoverBuilder = peer.discover().setInetAddress(inetAddress).setPorts(port);
            FutureDiscover futureDiscover = discoverBuilder.start();

//            final String address = inetAddress.toString()+":"+port;

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
//                String msg = "Put "+value.getObject().toString()+" under "+name;
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
    public void work(String projectName, String taskName) {
        taskManager.startTask(projectName, taskName, this);
    }

    @Override
    public List<PeerAddress> getNeighbours(){

        return neighbours;
    }

    @Override
    public List<PeerAddress> getOldNeighbours(){

        return oldNeighbours;
    }


    @Override
    public void reBootstrap() {

        ArrayList<PeerAddress> allNeighbours = new ArrayList<>();

        allNeighbours.addAll(neighbours);
        allNeighbours.addAll(oldNeighbours);

        for (PeerAddress p : allNeighbours) {
            bootstrap(p.getInetAddress().getHostAddress(),p.portTCP());
        }

    }
}
