package se.chalmers.gdcn.network;

/**
 * Created by Leif on 2014-03-29.
 */
public interface OnReplyCommand {
    void execute(Object replyMessageContent);
}
