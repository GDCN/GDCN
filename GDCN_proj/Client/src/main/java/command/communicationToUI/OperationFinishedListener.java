package command.communicationToUI;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * Created by HalfLeif on 2014-04-02.
 */

public abstract class OperationFinishedListener implements PropertyChangeListener {

    private final NetworkInterface client;
    private final Object resultKey;
    private final CommandWord commandWord;

    /**
     * Listener that will delete itself from the ClientInterface it was added to.
     *
     * @param client ClientInterface, will remove itself from this one
     * @param resultKey Key to listen for
     * @param commandWord CommandWord to listen for
     */
    public OperationFinishedListener(NetworkInterface client, Object resultKey, CommandWord commandWord) {
        this.client = client;
        this.resultKey = resultKey;
        this.commandWord = commandWord;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if(!(evt instanceof OperationFinishedEvent)){
            return;
        }
        OperationFinishedEvent event = (OperationFinishedEvent) evt;
        if( event.getCommandWord() != commandWord){
            return;
        }
        if(event.getOperation().getKey().equals(resultKey)){
            operationFinished(event.getOperation());
            client.removeListener(this);
        }
    }

    protected abstract void operationFinished(Operation operation);
}
