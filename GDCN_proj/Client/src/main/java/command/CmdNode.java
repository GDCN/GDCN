package command;

import net.tomp2p.futures.*;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.p2p.builder.BootstrapBuilder;
import net.tomp2p.p2p.builder.DiscoverBuilder;
import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import org.apache.log4j.Logger;

import java.io.IOException;
import java.net.InetAddress;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.util.Iterator;

/**
 * Created by Leif on 2014-02-17.
 */
public class CmdNode {

    private static Logger logger = Logger.getLogger(CmdNode.class);

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

    /**
     * @deprecated
     * @param port
     * @param listener
     */
    public void discover2(final int port, final Listener<String> listener){
        FutureBootstrap futureBootstrap = peer.bootstrap().setBroadcast().setPorts(port).start();
        futureBootstrap.addListener(new BaseFutureAdapter<FutureBootstrap>() {
            @Override
            public void operationComplete(FutureBootstrap future) throws Exception {
                if(!future.isSuccess()){
                    listener.message(false, "Future Bootstrap broadcast failed");
                    return;
                }

                if(future.getBootstrapTo()==null){
                    listener.message(false, "Future Bootstrap didn't find any!");
                    return;
                }

                Iterator<PeerAddress> iterator = future.getBootstrapTo().iterator();
                final DiscoverBuilder discoverBuilder = peer.discover().setPeerAddress(iterator.next());
                FutureDiscover futureDiscover = discoverBuilder.start();
                futureDiscover.addListener(new BaseFutureAdapter<FutureDiscover>(){

                    @Override
                    public void operationComplete(FutureDiscover future) throws Exception {
                        if(!future.isSuccess()){
                            listener.message(false, "Failed to discover "+discoverBuilder.getPeerAddress());
                            return;
                        }

                        listener.message(true, "Successfully discovered "+discoverBuilder.getPeerAddress());
                    }
                });
            }
        });
    }

    /**
     * @deprecated
     * @param listener
     */
    public void discover(final Listener<String> listener){
        DiscoverBuilder discoverBuilder = peer.discover().setPeerAddress(peer.getPeerAddress());
        FutureDiscover futureDiscover = discoverBuilder.start();
        futureDiscover.addListener(new BaseFutureAdapter<FutureDiscover>(){
            @Override
            public void operationComplete(FutureDiscover future) throws Exception {
                if(!future.isSuccess()){
                    listener.message(false,"Future discover failed");
                    return;
                }

                FutureBootstrap futureBootstrap = peer.bootstrap().setPeerAddress(peer.getPeerAddress()).start();
                futureBootstrap.addListener(new BaseFutureAdapter<FutureBootstrap>(){

                    @Override
                    public void operationComplete(FutureBootstrap future) throws Exception {
                        if(!future.isSuccess()){
                            listener.message(false, "Future bootstrap failed");
                            return;
                        }
                        listener.message(true, "Bootstrap successful!\n"+future.getBootstrapTo().toString());
                    }
                });
            }
        });
    }

    public void bootstrap(final InetAddress inetAddress, final int port, final Listener<String> listener){
        DiscoverBuilder discoverBuilder = peer.discover().setInetAddress(inetAddress).setPorts(port);
        FutureDiscover futureDiscover = discoverBuilder.start();
        futureDiscover.addListener(new BaseFutureAdapter<FutureDiscover>() {
            @Override
            public void operationComplete(FutureDiscover future) throws Exception {
                if(!future.isSuccess()){
                    listener.message(false,"Future discover failed");
                    return;
                }

                BootstrapBuilder bootstrapBuilder = peer.bootstrap().setInetAddress(inetAddress).setPorts(port);
                FutureBootstrap futureBootstrap = bootstrapBuilder.start();
                futureBootstrap.addListener(new BaseFutureAdapter<FutureBootstrap>() {
                    @Override
                    public void operationComplete(FutureBootstrap future) throws Exception {
                        if(!future.isSuccess()){
                            listener.message(false, "Future bootstrap failed");
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
}
