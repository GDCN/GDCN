package se.chalmers.gdcn.replica;

import java.io.Serializable;
import java.util.Comparator;

/**
* Created by Leif on 2014-04-17.
 *
*/
class TaskComparator implements Comparator<TaskCompare>, Serializable {
    @Override
    public int compare(TaskCompare o1, TaskCompare o2) {
        if(o1.value()>o2.value()){
            return 1;
        } else if(o1.value()<o2.value()){
            return -1;
        } else{
            //negative so that order of "" for the request is sorted correctly
            return -o1.order().compareTo(o2.order());
        }
    }
}
