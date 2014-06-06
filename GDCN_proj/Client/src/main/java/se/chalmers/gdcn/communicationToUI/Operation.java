package se.chalmers.gdcn.communicationToUI;

/**
* Created by HalfLeif on 2014-02-26.
*/
public class Operation<E>{
    private final boolean success;
    private final E result;
    private final ErrorCode errorCode;
    private final Object key;
    private final String reason;

    private Operation(boolean success, E result, ErrorCode errorCode, Object key, String reason) {
        this.success = success;
        this.result = result;
        this.errorCode = errorCode;
        this.key = key;
        this.reason = reason;
    }

    /**
     * @return if the operation succeeded
     */
    public boolean isSuccess() {
        return success;
    }

    /**
     * @return result of operation
     */
    public E getResult() {
        return result;
    }

    /**
     * @return ErrorCode if failed, null otherwise
     */
    public ErrorCode getErrorCode() {
        return errorCode;
    }

    /**
     * @return key if the operation used any special key, null otherwise
     */
    public Object getKey() {
        return key;
    }

    /**
     * @return Reason for failure, null if succeeded
     */
    public String getReason() {
        return reason;
    }

    /**
     * Builder class for Operation<E>
     * @param <E> result type of Operation
     */
    public static class OperationBuilder<E>{

        private final boolean success;

        private E result = null;
        private ErrorCode errorCode = null;
        private Object key = "";
        private String reason = "";

        public OperationBuilder(boolean success){
            this.success = success;
        }

        public OperationBuilder<E> setResult(E result) {
            this.result = result;
            return this;
        }

        public OperationBuilder<E> setErrorCode(ErrorCode errorCode) {
            this.errorCode = errorCode;
            return this;
        }

        /**
         * @param key Typically String or Number160
         * @return Builder
         */
        public OperationBuilder<E> setKey(Object key) {
            this.key = key;
            return this;
        }

        /**
         * @param reason String for reason why the operation failed, null if succeeded.
         * @return Builder
         */
        public OperationBuilder<E> setReason(String reason) {
            this.reason = reason;
            return this;
        }

        /**
         * @return Operation instance to be used in events
         */
        public Operation<E> create(){
            return new Operation<>(success, result, errorCode, key, reason);
        }
    }
}
