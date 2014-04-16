package utils;

import java.util.Set;

/**
 * Created by HalfLeif on 2014-04-16.
 */
public class TestUtils {

    public static void nap(int millis){
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public static void print(Set<String> set){
        for(String s : set){
            System.out.println(s);
        }
    }
}
