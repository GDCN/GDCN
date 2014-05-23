package se.chalmers.gdcn.network;

import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.PeerAddress;
import se.chalmers.gdcn.communicationToUI.ClientInterface;
import se.chalmers.gdcn.deceitful.Deceitful;
import se.chalmers.gdcn.replica.ReplicaManager.ReplicaID;

import java.util.Random;

/**
 * Created by HalfLeif on 2014-05-23.
 */
public abstract class DeceitfulWork {

    protected final TaskPasser taskPasser;
    protected final ClientInterface client;
    protected final WorkerID myWorkerID;
    protected final Random random = new Random();

    public DeceitfulWork(ClientInterface client, TaskPasser taskPasser, Peer peer) {
        this.myWorkerID = new WorkerID(peer.getPeerBean().getKeyPair().getPublic());
        this.client = client;
        this.taskPasser = taskPasser;
    }

    @Deceitful
    public abstract void requestWork(PeerAddress jobOwner);

    @Deceitful
    protected final void notifyJobOwner(TaskPasser taskPasser, PeerAddress jobOwner, WorkerID myWorkerID, ReplicaID replicaID){
        taskPasser.sendNoReplyMessage(jobOwner, new TaskMessage(TaskMessageType.RESULT_UPLOADED, myWorkerID,
                replicaID));
    }
}
