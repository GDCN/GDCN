package se.chalmers.gdcn.utils;

/**
 * Formats strings for printing with in a terminal
 * Created by joakim on 5/21/14.
 */
public class FormatString {

    private static final String COLOUR = "\033[3";
    private static final String CMD_END = "m";
    private static final String NORMALIZE = "\033[0m";

    public enum Colour {
        BLACK(0),
        RED(1),
        GREEN(2),
        YELLOW(3),
        BLUE(4),
        PURPLE(5),
        CYAN(6),
        WHITE(7);

        private String c;

        private Colour(int n) {
            c = n + "";
        }

        private String colour() {
            return c;
        }
    }

    public static String colour(String text, Colour c) {
        return startColour(c) + text + NORMALIZE;
    }

    private static String startColour(Colour c) {
        return COLOUR + c.colour() + CMD_END;
    }
}
