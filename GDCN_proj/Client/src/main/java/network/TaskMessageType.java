package network;

/**
 * Specific enum used for this message passing interface
 */
enum TaskMessageType {
    REQUEST_CHALLENGE,
    CHALLENGE,
    REQUEST_TASK,
    TASK_FAIL,
    CHALLENGE_FAIL,
    TASK,
    NO_TASK_AVAILABLE,
    RESULT_UPLOADED,
    HELLO
}
