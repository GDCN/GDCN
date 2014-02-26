package command.communicationToUI;

import java.beans.PropertyChangeEvent;

/**
 * Created by HalfLeif on 2014-02-26.
 */
public class OperationFinishedEvent<E> extends PropertyChangeEvent {
    /**
     * Constructs a new <code>PropertyChangeEvent</code>.
     *
     * @param source       The bean that fired the event.
     * @param commandWord     The old value of the property.
     * @param operationResult     The new value of the property.
     */
    public OperationFinishedEvent(Object source, CommandWord commandWord, Operation<E> operationResult) {
        super(source, null, commandWord, operationResult);
    }

    @Override
    public CommandWord getOldValue(){
        return (CommandWord) super.getOldValue();
    }

    @Override
    public Operation<E> getNewValue(){
        return (Operation<E>) super.getNewValue();
    }

}
