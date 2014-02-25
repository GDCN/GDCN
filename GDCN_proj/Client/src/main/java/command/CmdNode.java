package command;

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
public class CmdNode {

    private final Peer peer;

    public CmdNode(int port) throws NoSuchAlgorithmException, IOException {
        KeyPairGenerator generator = KeyPairGenerator.getInstance("DSA");
        KeyPair keyPair = generator.generateKeyPair();

        peer = new PeerMaker( keyPair).setPorts(port).makeAndListen();
    }

    public boolean isShutdown(){
        return peer.isShutdown();
    }

    public void shutdown(){
        if(!isShutdown()){
            peer.shutdown();
        }
    }

    public void bootstrap(final InetAddress inetAddress, final int port, final Listener<String> listener){
        DiscoverBuilder discoverBuilder = peer.discover().setInetAddress(inetAddress).setPorts(port);
        FutureDiscover futureDiscover = discoverBuilder.start();

        final String address = inetAddress.toString()+":"+port;

        futureDiscover.addListener(new BaseFutureAdapter<FutureDiscover>() {
            @Override
            public void operationComplete(FutureDiscover future) throws Exception {
                if(!future.isSuccess()){
                    listener.message(false,"Future discover failed\n"+address);
                    return;
                }

                BootstrapBuilder bootstrapBuilder = peer.bootstrap().setInetAddress(inetAddress).setPorts(port);
                FutureBootstrap futureBootstrap = bootstrapBuilder.start();
                futureBootstrap.addListener(new BaseFutureAdapter<FutureBootstrap>() {
                    @Override
                    public void operationComplete(FutureBootstrap future) throws Exception {
                        if(!future.isSuccess()){
                            listener.message(false, "Future bootstrap failed\n"+address);
                            return;
                        }
                        listener.message(true, "Bootstrap successful!\n"+future.getBootstrapTo().toString());
                    }
                });
            }
        });
    }

    public void put(final String name, final Data value, final Listener<String> listener){
        FutureDHT futureDHT = peer.put(Number160.createHash(name)).setData(value).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>(){

            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                boolean success = future.isSuccess();
                String msg = "Put "+value.getObject().toString()+" under "+name;
                if(success){
                    listener.message(success, msg);
                } else {
                    listener.message(success, "Failed to "+msg);
                }

            }
        });
    }

    public void get(final String name, final Listener<Data> listener){
        FutureDHT futureDHT = peer.get(Number160.createHash(name)).start();
        futureDHT.addListener(new BaseFutureAdapter<FutureDHT>() {
            @Override
            public void operationComplete(FutureDHT future) throws Exception {
                if(future.isSuccess()){
                    listener.message(true, future.getData());
                } else {
                    listener.message(false, null);
                }
            }
        });

    }

    public List<PeerAddress> getNeighbors(final Listener<String> listener){

        List<PeerAddress> peers = peer.getPeerBean().getPeerMap().getAll();
        String message = "Neighbors: ";
        if(peers.isEmpty()) {
            message = message + "none";
        } else {
            for (PeerAddress p : peers) {
//                message = message + p.getInetAddress() + "\n";
                message = message + p.toString() + "\n";
            }
        }
        message = message.trim();
        listener.message(true, message);

        return peers;
    }

    public void reBootstrap(List<PeerAddress> peers, final Listener<String> listener) {

        for (PeerAddress p : peers) {
            try {
                bootstrap(InetAddress.getByName(p.getInetAddress().getHostAddress()),p.portTCP(), listener);
            } catch (UnknownHostException e) {
                listener.message(false, "Node not active + \n" + p.toString());
            }
        }

    }
}
