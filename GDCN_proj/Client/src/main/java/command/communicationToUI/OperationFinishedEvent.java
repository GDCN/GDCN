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
     * @param propertyName The programmatic name of the property
     *                     that was changed.
     * @param oldValue     The old value of the property.
     * @param newValue     The new value of the property.
     */
    public OperationFinishedEvent(Object source, String propertyName, boolean oldValue, Operation<E> newValue) {
        super(source, propertyName, oldValue, newValue);
    }

    @Override
    public Boolean getOldValue(){
        return (Boolean) super.getOldValue();
    }

    @Override
    public E getNewValue(){
        return (E) super.getNewValue();
    }
    
}
