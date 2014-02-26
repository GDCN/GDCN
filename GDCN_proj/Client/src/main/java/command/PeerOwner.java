package command;

import command.communicationToUI.ResultCode;
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
import java.beans.PropertyChangeSupport;
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
    private PropertyChangeSupport notifier = new PropertyChangeSupport(this);

    public static class Operation<E>{
        private boolean success;
        private E result;

        public Operation(boolean success, E result) {
            this.success = success;
            this.result = result;
        }

        public boolean isSuccess() {
            return success;
        }

        public E getResult() {
            return result;
        }
    }

    @Override
    public void addListener(PropertyChangeListener listener){
        notifier.addPropertyChangeListener(listener);
    }

    @Override
    public void removeListener(PropertyChangeListener listener){
        notifier.removePropertyChangeListener(listener);
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
        //TODO notify UI
    }

    @Override
    public void stop(){
        if(peer == null){
            //TODO alert UI?
            return;
        }
        if(!peer.isShutdown()){
            //TODO alert UI?
            peer.shutdown();
        }
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
                        notifier.firePropertyChange("Bootstrap", false, ResultCode.DISCOVER_FAILURE);
                        return;
                    }

                    BootstrapBuilder bootstrapBuilder = peer.bootstrap().setInetAddress(inetAddress).setPorts(port);
                    FutureBootstrap futureBootstrap = bootstrapBuilder.start();
                    futureBootstrap.addListener(new BaseFutureAdapter<FutureBootstrap>() {
                        @Override
                        public void operationComplete(FutureBootstrap future) throws Exception {
                            if(!future.isSuccess()){
                                notifier.firePropertyChange("Bootstrap", false, ResultCode.BOOTSTRAP_FAILURE);
                                return;
                            }
                            notifier.firePropertyChange("Bootstrap", true, new Operation<InetAddress>(true,inetAddress));
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
//                notifier.firePropertyChange("Put", null, null);
                if(success){
//                    listener.message(success, msg);
                } else {
//                    listener.message(success, "Failed to "+msg);
                }

            }
        });
    }

    @Override
    public void get(final String name){
        FutureDHT futureDHT = peer.get(Number160.createHash(name)).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {
            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                if(future.isSuccess()){
//                    listener.message(true, future.getData());
                } else {
//                    listener.message(false, null);
                }
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
