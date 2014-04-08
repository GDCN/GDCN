package unitTests;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import replica.Outdater;
import replica.ReplicaTimer;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

/**
 * Created by HalfLeif on 2014-04-08.
 */
public class ReplicaTimerTest {

    private ReplicaTimer replicaTimer;
    private final Outdater outdater = new Outdater() {
        @Override
        public void replicaOutdated(String replicaID) {
            System.out.println("Outdate: "+replicaID);
            outdatedCalled++;
        }
    };

    private final String replicaA = "id A";
    private final String replicaB = "id B";

    private volatile int outdatedCalled;

    @BeforeMethod
    public void setupMethod(){

        outdatedCalled = 0;
    }

    @Test
    public void testDate(){
        Date before = futureDate(0);
        Date after = futureDate(500);
        assert before.compareTo(after) < 0;
    }

    @Test
    public void singleUpdateTest(){
        replicaTimer = new ReplicaTimer(outdater, 100);
        assert outdatedCalled == 0;
        start(replicaTimer);

        replicaTimer.add(replicaA, futureDate(500));
        nap(200);

        assert outdatedCalled == 0;
        nap(400);

        assert outdatedCalled == 1;
    }

    private static void nap(int millis){
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private static void start(ReplicaTimer replicaTimer){
        Thread timerThread = new Thread(replicaTimer.createUpdater());
        timerThread.setDaemon(true);

        timerThread.start();
    }

    private static Date futureDate(int millis){
        Calendar calendar = new GregorianCalendar();
        calendar.add(Calendar.MILLISECOND, millis);
        return calendar.getTime();
    }
}
