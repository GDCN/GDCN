package command.communicationToUI;

/**
* Created by HalfLeif on 2014-02-26.
*/
public class Operation<E>{
    private final boolean success;
    private final E result;
    private final ErrorCode errorCode;
    private final String key;

    public Operation(boolean success, E result, ErrorCode errorCode, String key) {
        this.success = success;
        this.result = result;
        this.errorCode = errorCode;
        this.key = key;
    }

    public boolean isSuccess() {
        return success;
    }

    public E getResult() {
        return result;
    }

    public ErrorCode getErrorCode() {
        return errorCode;
    }

    public String getKey() {
        return key;
    }

    public static class OperationBuilder<E>{

        private final boolean success;

        private E result = null;
        private ErrorCode errorCode = null;
        private String key = "";

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

        public OperationBuilder<E> setKey(String key) {
            this.key = key;
            return this;
        }

        public Operation<E> create(){
            return new Operation<E>(success, result, errorCode, key);
        }
    }
}
