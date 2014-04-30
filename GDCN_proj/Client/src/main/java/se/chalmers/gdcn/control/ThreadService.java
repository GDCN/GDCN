package se.chalmers.gdcn.control;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

/**
 * Created by HalfLeif on 2014-04-29.
 */
public class ThreadService {
    private final ExecutorService threadPool = Executors.newFixedThreadPool(4, new ThreadFactory() {
        @Override
        public Thread newThread(Runnable r) {
            Thread thread = new Thread(r);
            thread.setDaemon(true);
            return thread;
        }
    });

    private static ThreadService instance = null;

    private static void check(){
        if(instance == null){
            instance = new ThreadService();
        }
    }

    public static void submit(Runnable runnable){
        check();
        instance.threadPool.submit(runnable);
    }
}
