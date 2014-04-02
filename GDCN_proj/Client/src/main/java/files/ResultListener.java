package files;

/**
 * Created by HalfLeif on 2014-03-25.
 *
 * TODO remove entirely?
 */
public interface ResultListener {

    /**
     *
     * @param results Serialized result file. Null if task failed
     */
    public void taskCompleted(byte[] results);
}
