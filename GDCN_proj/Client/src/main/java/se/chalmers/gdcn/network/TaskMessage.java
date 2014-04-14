package se.chalmers.gdcn.network;

import java.io.Serializable;

/**
 * Class for encapsulating task messages. It is similar to {@link NetworkMessage}
 * but more specific for this purpose. NetworkMessage can be used for any purpose and will in this case
 * contain an object of TaskMessage.
 */
class TaskMessage implements Serializable {
    private final TaskMessageType type;
    private final WorkerID senderID;
    private final Object actualContent;

    TaskMessage(TaskMessageType type, WorkerID senderID, Object actualContent) {
        this.type = type;
        this.senderID = senderID;
        this.actualContent = actualContent;
    }

    public static TaskMessage check(Object messageContent){
        if(!(messageContent instanceof TaskMessage)){
            throw new IllegalStateException("Message from is not a TaskMessage! "+messageContent.toString());
        }

        return (TaskMessage) messageContent;
    }

    public TaskMessageType getType() {
        return type;
    }

    public WorkerID getSenderID() {
        return senderID;
    }

    public Object getActualContent() {
        return actualContent;
    }

    @Override
    public String toString() {
        return "TaskMsg{ " + type +
                ", " + actualContent +
                '}';
    }
}
