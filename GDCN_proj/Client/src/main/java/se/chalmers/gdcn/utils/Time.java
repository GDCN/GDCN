package se.chalmers.gdcn.utils;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

/**
 * Type safe time unit mapping to Calendar constants.
 */
public enum Time {
    MILLISECOND(Calendar.MILLISECOND, 1),
    SECOND(Calendar.SECOND, 1000),
    MINUTE(Calendar.MINUTE, 60*SECOND.comparedToMillis),
    HOUR(Calendar.HOUR, 3600*SECOND.comparedToMillis),
    ;
    private final int typeConstant;
    private final long comparedToMillis;

    Time(int typeConstant, long comparedToMillis) {
        this.typeConstant = typeConstant;
        this.comparedToMillis = comparedToMillis;
    }

    public int getTypeConstant() {
        return typeConstant;
    }

    public long getComparedToMillis() {
        return comparedToMillis;
    }

    public static Date futureDate(Time unit, int value){
        Calendar calendar = new GregorianCalendar();
        calendar.add(unit.typeConstant, value);
        return calendar.getTime();
    }
}
