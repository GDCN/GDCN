package unitTests;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import se.chalmers.gdcn.utils.SerializableTimer;

import java.util.*;

/**
 * Created by Leif on 2014-04-16.
 */
public class TimerTest {

    private final String stringA = "id A";
    private final String stringB = "id B";
    private final String stringC = "id C";

    private final Set<String> timeouts = new HashSet<>();
    private TestTimer timer;
    
    @BeforeMethod
    public void setupMethod(){
        timeouts.clear();
    }
    
    @Test
    public void testPositive(){
        timer = new TestTimer(30);
        timer.add(stringA, futureDate(60));
        assert timeouts.size()==0;
        start(timer);
        nap(100);
        assert timeouts.contains(stringA);
    }
    
    private class TestTimer extends SerializableTimer<String>{
        /**
         * @param updateTime Number of Milliseconds between check queue
         */
        public TestTimer(long updateTime) {
            super(updateTime);
        }

        @Override
        protected void handleTimeout(String element) {
            timeouts.add(element);
        }
    }

    private static Date futureDate(int millis){
        Calendar calendar = new GregorianCalendar();
        calendar.add(Calendar.MILLISECOND, millis);
        return calendar.getTime();
    }

    private static void nap(int millis){
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private static Thread start(SerializableTimer serializableTimer){
        Thread timerThread = new Thread(serializableTimer.createUpdater());
        timerThread.setDaemon(true);
        
        timerThread.start();
        return timerThread;
    }
}
