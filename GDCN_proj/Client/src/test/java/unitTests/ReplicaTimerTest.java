package unitTests;

import net.tomp2p.storage.Data;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import se.chalmers.gdcn.replica.Outdater;
import se.chalmers.gdcn.replica.ReplicaTimer;

import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

/**
 * Created by HalfLeif on 2014-04-08.
 *
 * @deprecated
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
    private final String replicaC = "id C";

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

    @Test
    public void multiUpdateTest(){
        replicaTimer = new ReplicaTimer(outdater, 50);
        start(replicaTimer);

        replicaTimer.add(replicaA, futureDate(-1));
        replicaTimer.add(replicaB, futureDate(150));
        replicaTimer.add(replicaC, futureDate(150));

        nap(50);
        assert outdatedCalled == 1;

        nap(130);
        assert outdatedCalled == 3;
    }

    @Test
    public void manyMultiUpdateTest(){
        replicaTimer = new ReplicaTimer(outdater, 50);
        start(replicaTimer);

        final int times = 100;

        for(int i=0; i<times; ++i){
            replicaTimer.add("Id_"+i, futureDate(300+i%5));
        }

        nap(350);
        assert outdatedCalled == times;
    }

    @Test
    public void deserializeTest() throws IOException, ClassNotFoundException {
        replicaTimer = new ReplicaTimer(outdater, 50);
        start(replicaTimer);

        replicaTimer.add(replicaA, futureDate(500));
        replicaTimer.add(replicaB, futureDate(-1));
        Data serialized = new Data(replicaTimer.clone());

        ReplicaTimer deserialized = (ReplicaTimer) serialized.getObject();
        deserialized.setOutdater(outdater);
        start(deserialized);

        nap(50);
        assert outdatedCalled == 2;
        nap(500);
        assert outdatedCalled == 4;
    }

    private static void nap(int millis){
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private static Thread start(ReplicaTimer replicaTimer){
        Thread timerThread = new Thread(replicaTimer.createUpdater());
        timerThread.setDaemon(true);

        timerThread.start();
        return timerThread;
    }

    private static Date futureDate(int millis){
        Calendar calendar = new GregorianCalendar();
        calendar.add(Calendar.MILLISECOND, millis);
        return calendar.getTime();
    }
}
