package se.chalmers.gdcn.replica;

/**
 * Created by HalfLeif on 2014-04-15.
 *
 * Interface for ordering in a sorted collection.
 */
public interface TaskCompare{
    /**
     * @return Primary ordering attribute
     */
    float value();

    /**
     * @return Secondary ordering attribute
     */
    String order();
}
