package se.chalmers.gdcn.utils;

import java.io.Serializable;
import java.util.*;

/**
 * Created by Leif on 2014-04-16.
 */
public abstract class SerializableTimer<E> implements Serializable {

    private final long UPDATE_TIME;
    private final PriorityQueue<Timeout<E>> queue = new PriorityQueue<>();

    /**
     * @param updateTime Number of Milliseconds between check queue
     */
    public SerializableTimer(long updateTime) {
        UPDATE_TIME =  updateTime;
    }

    /**
     * Clock that updates this timer. This class must be Serializable which {@link java.util.Timer} isn't.
     * @return Runnable
     */
    public final Runnable createUpdater(){
        return new Runnable() {
            @Override
            public void run() {
                Timer timer = new Timer(true);
                timer.schedule(new TimerTask() {
                    @Override
                    public void run() {
                        update();
                    }
                }, UPDATE_TIME/2, UPDATE_TIME);
            }
        };
    }

    public final synchronized void add(E element, Date date){
        Timeout<E> timeout = new Timeout<>(element, date);
        queue.add(timeout);
    }

    public final synchronized boolean remove(E element){
        //Should work since Timeout equals only depend on element
        Timeout<E> timeout = new Timeout<>(element, null);
        return queue.remove(timeout);
    }

    /**
     * Called by clock to check the queue.
     */
    private synchronized void update(){
        final Date currentTime = new Date();
        if(queue.peek()==null){
            //Queue empty, ignore
            return;
        }
        while(queue.peek()!=null && queue.peek().getDate().compareTo(currentTime) < 0){
            Timeout<E> outdated = queue.remove();
            handleTimeout(outdated.element);
        }
    }

    protected abstract void handleTimeout(E element);

    private static class Timeout<E> implements Serializable, Comparable<Timeout>{

        private final Date date;
        private final E element;

        private Timeout(E element, Date date) {
            this.date = date;
            this.element = element;
        }

        public E getElement() {
            return element;
        }

        public Date getDate() {
            return date;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Timeout)) return false;

            Timeout timeout = (Timeout) o;

            if (!element.equals(timeout.element)) return false;

            return true;
        }

        @Override
        public int hashCode() {
            return element.hashCode();
        }

        /**
         *
         * @param replicaTimeout Other ReplicaTimer2
         * @return comparison
         */
        @Override
        public int compareTo(Timeout replicaTimeout) {
            if(replicaTimeout==null){
                return 1;
            }
            return date.compareTo(replicaTimeout.date);
        }
    }



}
