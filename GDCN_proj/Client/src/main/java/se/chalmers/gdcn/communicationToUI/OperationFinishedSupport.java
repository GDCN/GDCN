package se.chalmers.gdcn.communicationToUI;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

/**
 * Created by HalfLeif on 2014-02-26.
 */
public class OperationFinishedSupport {

    private final PropertyChangeSupport propertyChangeSupport;

    private final Object source;

    /**
     * Constructs a <code>PropertyChangeSupport</code> object.
     *
     * @param sourceBean The bean to be given as the source for any events.
     */
    public OperationFinishedSupport(Object sourceBean) {
        this.source = sourceBean;
        this.propertyChangeSupport = new PropertyChangeSupport(sourceBean);
    }

    /**
     *
     * Fire new event of an operation finished
     * @param event event to fire
     */
    public void fireOperationFinished(OperationFinishedEvent event){
        propertyChangeSupport.firePropertyChange(event);
    }

    /**
     * Fire new event of an operation finished
     * @param commandWord kind of command
     * @param operation operation instance
     */
    public void fireOperationFinished(CommandWord commandWord, Operation operation){
        this.fireOperationFinished(new OperationFinishedEvent(source, commandWord, operation));
    }

    /**
     * Add new listener
     * @param listener listener
     */
    public void addListener(PropertyChangeListener listener){
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    /**
     * Remove previously added listener
     * @param listener listener
     */
    public void removeListener(PropertyChangeListener listener){
        propertyChangeSupport.removePropertyChangeListener(listener);
    }
}
