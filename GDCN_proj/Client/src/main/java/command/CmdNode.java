package command;

import net.tomp2p.futures.BaseFutureAdapter;
import net.tomp2p.futures.FutureBootstrap;
import net.tomp2p.futures.FutureDHT;
import net.tomp2p.futures.FutureDiscover;
import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.p2p.builder.DiscoverBuilder;
import net.tomp2p.peers.Number160;
import net.tomp2p.storage.Data;
import org.apache.log4j.Logger;

import java.io.IOException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

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
