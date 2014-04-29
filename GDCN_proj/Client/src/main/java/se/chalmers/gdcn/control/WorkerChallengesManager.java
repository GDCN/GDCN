package se.chalmers.gdcn.control;

import se.chalmers.gdcn.hashcash.Solution;
import se.chalmers.gdcn.network.WorkerID;

import java.security.SecureRandom;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Created by weeeeeew on 2014-04-29.
 */
public class WorkerChallengesManager {
    private final Map<WorkerID, Integer> registeredWorkers = new ConcurrentHashMap<>();
    private final Random random = new SecureRandom();

    public void solvedChallenge(WorkerID worker, Solution solution) {
        if (!registeredWorkers.containsKey(worker)) {
            return;
        }

        Integer score = registeredWorkers.get(worker);

        score += random.nextInt(score + solution.getDifficulty()) - solution.getDifficulty();
        registeredWorkers.put(worker,score);
    }

    public boolean newWorker(WorkerID worker) {
        if(registeredWorkers.containsKey(worker)) {
            return false;
        } else {
            registeredWorkers.put(worker, random.nextInt(Integer.MAX_VALUE/4));
            return true;
        }
    }

    public int getCurrentScore(WorkerID worker) {
        newWorker(worker);
        return registeredWorkers.get(worker);
    }
}
