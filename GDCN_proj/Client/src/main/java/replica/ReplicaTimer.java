package replica;

import java.io.Serializable;
import java.util.Date;
import java.util.PriorityQueue;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Created by HalfLeif on 2014-04-08.
 */
public class ReplicaTimer implements Serializable {

    private final static long UPDATE_TIME = 1000*60;

    private final PriorityQueue<ReplicaTimeout> queue = new PriorityQueue<>();
    private final Outdater outdater;

    public ReplicaTimer(Outdater outdater) {
        this.outdater = outdater;
    }

    /**
     * Clock that updates this timer. This class must be Serializable which {@link java.util.Timer} isn't.
     * @return Runnable
     */
    public Runnable createUpdater(){
        return new Runnable() {
            @Override
            public void run() {
                Timer timer = new Timer(true);
                timer.schedule(new TimerTask() {
                    @Override
                    public void run() {
                        update();
                    }
                }, UPDATE_TIME);
            }
        };
    }

    /**
     *
     * @param replicaID ID of a replica
     * @param date Expiration date of the replica
     */
    public synchronized void add(String replicaID, Date date){
        ReplicaTimeout replicaTimeout = new ReplicaTimeout(replicaID, date);
        queue.add(replicaTimeout);
    }

    /**
     * Called by clock to check the queue.
     *
     * Resembles busy-wait. Problem is, {@link java.util.Timer} is not serializable...
     */
    private synchronized void update(){
        final Date currentTime = new Date();
        while(queue.peek()!=null && queue.peek().getDate().compareTo(currentTime) < 0){
            ReplicaTimeout outdated = queue.poll();
            outdater.replicaOutdated(outdated.getReplicaID());
        }
    }


    private static class ReplicaTimeout implements Serializable, Comparable<ReplicaTimeout>{

        private final Date date;
        private final String replicaID;

        private ReplicaTimeout(String replicaID, Date date) {
            this.date = date;
            this.replicaID = replicaID;
        }

        public String getReplicaID() {
            return replicaID;
        }

        public Date getDate() {
            return date;
        }

        @Override
        public int compareTo(ReplicaTimeout replicaTimeout) {
            return date.compareTo(replicaTimeout.date);
        }
    }
}
