package network;

import net.tomp2p.connection.ConnectionBean;
import net.tomp2p.connection.PeerBean;
import net.tomp2p.message.Message;
import net.tomp2p.p2p.BroadcastHandler;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.Number160;
import net.tomp2p.rpc.BroadcastRPC;
import net.tomp2p.storage.Data;

import java.util.Map;

/**
 * Created by Leif on 2014-03-19.
 */
public class MessagePasser {

    /**
     * Sets BroadcastRPC to our own extended version in order to handle custom message passing.
     * @param node
     */
    public MessagePasser(Peer node){
        node.setBroadcastRPC(new BRPC(node.getPeerBean(), node.getConnectionBean(), handler));
    }

    /**
     * See tomp2p implementation at {@link net.tomp2p.p2p.DefaultBroadcastHandler#receive(net.tomp2p.message.Message)}
     */
    private BroadcastHandler handler = new BroadcastHandler() {
        @Override
        public void receive(Message message) {
            final Number160 messageKey = message.getKey();
            final Map<Number160, Data> dataMap = message.getDataMap();
            final int hopCount = message.getInteger();
            //TODO
        }
    };

    private class BRPC extends BroadcastRPC {
        public BRPC(PeerBean peerBean, ConnectionBean connectionBean, BroadcastHandler broadcastHandler) {
            super(peerBean, connectionBean, broadcastHandler);
        }

        @Override
        public Message handleResponse(Message message, boolean sign) throws Exception {
            Message.Command cmd = message.getCommand();
            if (! (cmd == Message.Command.USER1 || cmd == Message.Command.USER2)) {
                super.handleResponse(message, sign);
            }
            handler.receive(message);
            return message;
        }
    }
}
