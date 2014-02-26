package command;

import command.communicationToUI.ErrorCode;
import command.communicationToUI.Operation;
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
import net.tomp2p.storage.Data;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.List;

/**
 * Created by Leif on 2014-02-17.
 */
public class PeerOwner implements command.communicationToUI.ClientInterface {

    private Peer peer  = null;
    private List<PeerAddress> neighbours;
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
        try {

            KeyPairGenerator generator = KeyPairGenerator.getInstance("DSA");
            KeyPair keyPair = generator.generateKeyPair();
            peer = new PeerMaker( keyPair).setPorts(port).makeAndListen();

        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        notifier.fireOperationFinished("start", peer != null,
                new OperationBuilder<Integer>().setResult(port).create());
    }

    @Override
    public void stop(){
        if(peer == null){
            notifier.fireOperationFinished("stop", false,
                    new OperationBuilder().setErrorCode(ErrorCode.NOT_CONNECTED).setSuccess(false).create());
            return;
        }
        if(!peer.isShutdown()){
            peer.shutdown();
        }
        notifier.fireOperationFinished("stop", true, new OperationBuilder().setSuccess(true).create());
    }

    @Override
    public void bootstrap(final String host, final int port){
        try {
            final InetAddress inetAddress = InetAddress.getByName(host);

            DiscoverBuilder discoverBuilder = peer.discover().setInetAddress(inetAddress).setPorts(port);
            FutureDiscover futureDiscover = discoverBuilder.start();

            final String address = inetAddress.toString()+":"+port;

            futureDiscover.addListener(new BaseFutureAdapter<FutureDiscover>() {
                @Override
                public void operationComplete(FutureDiscover future) throws Exception {
                    if(!future.isSuccess()){
                        notifier.fireOperationFinished("Bootstrap", false,
                                new OperationBuilder<InetAddress>().setSuccess(false).setErrorCode(ErrorCode.DISCOVER_FAILURE).create());
                        return;
                    }

                    BootstrapBuilder bootstrapBuilder = peer.bootstrap().setInetAddress(inetAddress).setPorts(port);
                    FutureBootstrap futureBootstrap = bootstrapBuilder.start();
                    futureBootstrap.addListener(new BaseFutureAdapter<FutureBootstrap>() {
                        @Override
                        public void operationComplete(FutureBootstrap future) throws Exception {
                            if(!future.isSuccess()){
                                notifier.fireOperationFinished("Bootstrap", false,
                                        new OperationBuilder<InetAddress>().setSuccess(false).setResult(inetAddress).setErrorCode(ErrorCode.BOOTSTRAP_FAILURE).create());
                                return;
                            }
                            notifier.fireOperationFinished("Bootstrap", true,
                                    new OperationBuilder<InetAddress>().setSuccess(true).setResult(inetAddress).create());
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
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>(){

            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                boolean success = future.isSuccess();
                String msg = "Put "+value.getObject().toString()+" under "+name;
                Operation<Data> result = new OperationBuilder<Data>().setResult(value).setKey(name).setSuccess(success).create();
                notifier.fireOperationFinished("Put", success, result);
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
                notifier.fireOperationFinished("get", success,
                        new OperationBuilder<Data>().setSuccess(success).setKey(name).setResult(future.getData()).create());
            }
        });

    }

    @Override
    public List<PeerAddress> getNeighbours(){

        List<PeerAddress> peers = peer.getPeerBean().getPeerMap().getAll();
//        String message = "Neighbors: ";
//        if(peers.isEmpty()) {
//            message = message + "none";
//        } else {
//            for (PeerAddress p : peers) {
////                message = message + p.getInetAddress() + "\n";
//                message = message + p.toString() + "\n";
//            }
//        }
//        message = message.trim();
////        listener.message(true, message);

        return peers;
    }

    @Override
    public void reBootstrap(List<PeerAddress> peers) {
        for (PeerAddress p : peers) {
            bootstrap(p.getInetAddress().getHostAddress(),p.portTCP());
        }

    }
}
