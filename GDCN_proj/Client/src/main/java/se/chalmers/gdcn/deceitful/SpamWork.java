package se.chalmers.gdcn.deceitful;

import net.tomp2p.p2p.Peer;
import net.tomp2p.p2p.PeerMaker;
import net.tomp2p.peers.PeerAddress;
import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.control.TaskManager;
import se.chalmers.gdcn.files.DataFilesManager;
import se.chalmers.gdcn.network.AbstractDeceitfulWork;
import se.chalmers.gdcn.network.TaskPasser;
import se.chalmers.gdcn.network.TaskPasser.WorkMethod;
import se.chalmers.gdcn.replica.ReplicaBox;

import java.io.IOException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

/**
 * Created by HalfLeif on 2014-05-23.
 */
public class SpamWork implements DeceitfulWork{

    private final TaskManager taskManager;
    private final ClientInterface client;

    private final Peer[] peers;
    private final SpamWorkerNode[] spamWorkerNodes;
    private KeyPairGenerator keyPairGenerator = null;

    private int startPort = 37000;
    private long timeout = 25000L;

    public SpamWork(int times, TaskManager taskManager, ClientInterface client) {
        this.taskManager = taskManager;
        this.client = client;
        this.peers = new Peer[times];
        this.spamWorkerNodes = new SpamWorkerNode[times];

        try {
            keyPairGenerator = KeyPairGenerator.getInstance("RSA");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    public void setStartPort(int startPort) {
        this.startPort = startPort;
    }

    public void setTimeout(long timeout) {
        this.timeout = timeout;
    }

    @Deceitful
    @Override
    public void requestWork(final PeerAddress jobOwner) {
        try {
            for(int ix=0; ix<peers.length; ++ix){
                KeyPair keyPair = keyPairGenerator.generateKeyPair();
                Peer peer = new PeerMaker(keyPair).setPorts(startPort+ix).makeAndListen();
                DataFilesManager dataFilesManager = new DataFilesManager("DECEIT",""+ix);
                TaskPasser taskPasser = new TaskPasser(peer, taskManager, client, dataFilesManager);

                final SpamWorkerNode spamWorkerNode = new SpamWorkerNode(client, taskPasser, peer);
                spamWorkerNodes[ix] = spamWorkerNode;

                taskManager.submit(new Runnable() {
                    @Override
                    public void run() {
                        spamWorkerNode.requestWork(jobOwner);
                    }
                });
            }
            taskManager.submit(new Runnable() {
                @Override
                public void run() {
                    try {
                        Thread.sleep(timeout);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    System.out.println("Shutdown all peers");
                    stop();
                }
            });
        } catch (IOException e) {
            e.printStackTrace();
            stop();
        }
    }

    private void stop(){
        for(Peer peer : peers){
            if(peer != null && !peer.isShutdown()){
                peer.shutdown();
            }
        }
    }

    private static class SpamWorkerNode extends AbstractDeceitfulWork{

        public SpamWorkerNode(ClientInterface client, TaskPasser taskPasser, Peer peer) {
            super(client, taskPasser, peer);
        }

        private WorkMethod workMethod = new WorkMethod() {
            @Override
            public void work(PeerAddress jobOwner, ReplicaBox replicaBox, boolean autoWork) {
                final String taskName = replicaBox.getTaskMeta().getTaskName();
                System.out.println("Task " + taskName + " finished. Will never notify job owner!");

                requestWork(jobOwner);
            }
        };

        @Override
        public void requestWork(PeerAddress jobOwner) {
            taskPasser.requestWork(jobOwner, true, workMethod);
        }
    }
}
