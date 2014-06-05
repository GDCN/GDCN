package se.chalmers.gdcn.hashcash;

import se.chalmers.gdcn.network.WorkerID;

import java.io.Serializable;
import java.security.SecureRandom;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Created by weeeeeew on 2014-04-29.
 */
public class WorkerChallengesManager implements Serializable{
    private final Map<WorkerID, Integer> registeredWorkers = new ConcurrentHashMap<>();
    private final Random random = new SecureRandom();

    /**
     * Registers that a worker has solved a challenge.
     * @param worker The worker that has solved a challenge.
     * @param solution The Solution the worker has solved.
     */
    public void solvedChallenge(WorkerID worker, Solution solution) {
        Integer score = getCurrentScore(worker);

        score += (random.nextInt(1+Math.abs(score + solution.getDifficulty())) - solution.getDifficulty()) % (Integer.MAX_VALUE/4);
        registeredWorkers.put(worker,score);
    }

    public boolean newWorker(WorkerID worker) {
        return !registeredWorkers.containsKey(worker);
    }

    public int getCurrentScore(WorkerID worker) {
        return newWorker(worker) ? 0 : registeredWorkers.get(worker);
    }
}
