package network;

import net.tomp2p.message.Message;
import net.tomp2p.p2p.BroadcastHandler;
import net.tomp2p.p2p.Peer;
import net.tomp2p.peers.Number160;
import net.tomp2p.rpc.ReplyHandler;
import net.tomp2p.storage.Data;

import java.util.Map;

/**
 * Created by Leif on 2014-03-19.
 */
public class MessagePasser {

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

    private class CustomReplyHandler extends ReplyHandler {

        public CustomReplyHandler(Peer node) {
            super(node.getPeerBean(), node.getConnectionBean());
        }

        @Override
        public Message handleResponse(Message message, boolean sign) throws Exception {
            Message.Command cmd = message.getCommand();
            if (! (cmd == Message.Command.USER1 || cmd == Message.Command.USER2)) {
                //TODO
            } else {
                //TODO
                handler.receive(message);
            }

            return message;
        }
    }
}
