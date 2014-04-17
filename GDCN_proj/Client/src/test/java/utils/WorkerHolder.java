package utils;

import se.chalmers.gdcn.network.WorkerID;

import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

/**
 * Created by Leif on 2014-04-17.
 */
public class WorkerHolder {

    private static WorkerID workerA;
    private static WorkerID workerB;
    private static WorkerID workerC;
    private static WorkerID myWorkerID;

    private static KeyPairGenerator generator;

    public static WorkerID getWorkerA() {
        if(workerA==null){
            workerA = generate();
        }
        return workerA;
    }

    public static WorkerID getWorkerB() {
        if(workerB==null){
            workerB = generate();
        }
        return workerB;
    }

    public static WorkerID getWorkerC() {
        if(workerC==null){
            workerC = generate();
        }
        return workerC;
    }

    public static WorkerID getMyWorkerID() {
        if(myWorkerID==null){
            myWorkerID = generate();
        }
        return myWorkerID;
    }

    private static WorkerID generate(){
        if(generator == null){
            try {
                generator = KeyPairGenerator.getInstance("RSA");
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
            }
        }
        return new WorkerID(generator.generateKeyPair().getPublic());
    }
}
