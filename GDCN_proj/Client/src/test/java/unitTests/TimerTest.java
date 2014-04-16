package unitTests;

import net.tomp2p.storage.Data;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import se.chalmers.gdcn.utils.SerializableTimer;

import java.io.IOException;
import java.io.Serializable;
import java.util.*;

/**
 * Created by Leif on 2014-04-16.
 */
public class TimerTest implements Serializable{

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
    public void testDate(){
        Date before = futureDate(0);
        Date after = futureDate(500);
        assert before.compareTo(after) < 0;
    }

    @Test
    public void singleUpdateTest(){
        timer = new TestTimer(30);
        timer.add(stringA, futureDate(60));
        assert timeouts.size()==0;
        start(timer);
        nap(100);
        assert timeouts.contains(stringA);
    }


    @Test
    public void pastTest(){
        timer = new TestTimer(25);
        start(timer);

        timer.add(stringA, futureDate(0));
        timer.add(stringB, futureDate(-100));
        timer.add(stringC, futureDate(-500));
        nap(30);
        assert timeouts.size() == 3;
    }

    @Test
    public void removeTest(){
        timer = new TestTimer(20);
        timer.add(stringA, futureDate(0));
        timer.add(stringB, futureDate(2));
        timer.add(stringC, futureDate(-10));

        timer.remove(stringA);

        start(timer);
        nap(30);
        assert ! timeouts.contains(stringA);
        assert timeouts.size() == 2;
    }

    @Test
    public void multiUpdateTest(){
        timer = new TestTimer(50);
        start(timer);

        timer.add(stringA, futureDate(-1));
        timer.add(stringB, futureDate(150));
        timer.add(stringC, futureDate(150));

        nap(50);
        assert timeouts.size() == 1;

        nap(130);
        assert timeouts.size() == 3;
    }

    @Test
    public void manyMultiUpdateTest(){
        timer = new TestTimer(50);
        start(timer);

        final int times = 100;

        for(int i=0; i<times; ++i){
            timer.add("Id_"+i, futureDate(300+i%5));
        }

        nap(350);
        assert timeouts.size() == times;
    }

    @Test
    public void deserializeCurrentTest() throws IOException, ClassNotFoundException {
        timer = new TestTimer(25);
        timer.add(stringB, futureDate(-1));
        Data serialized = new Data(this);

        TimerTest deserialized = (TimerTest) serialized.getObject();
        assert deserialized != this;
        assert deserialized.timeouts != this.timeouts;

        assert deserialized.timeouts.size() == 0;
        start(deserialized.timer);
        nap(30);
        assert deserialized.timeouts.contains(stringB);
    }

    @Test
    public void deserializeFutureTest() throws IOException, ClassNotFoundException {
        timer = new TestTimer(25);
        timer.add(stringA, futureDate(60));
        Data serialized = new Data(this);

        TimerTest deserialized = (TimerTest) serialized.getObject();
        assert deserialized != this;
        assert deserialized.timeouts != this.timeouts;

        start(deserialized.timer);
        nap(30);
        assert deserialized.timeouts.size() == 0;
        nap(100);
        assert deserialized.timeouts.contains(stringA);
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

    private static void print(Set<String> set){
        for(String s : set){
            System.out.println(s);
        }
    }
}
