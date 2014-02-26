package taskbuilder;

public class Utils {
    private static Boolean isWindows = null;

    public static char getCPSeparator() {
        return isWindows() ? ';' : ':';
    }

    public static boolean isWindows() {
        if (isWindows == null) {
            isWindows = System.getProperty("os.name").startsWith("Windows");
        }

        return isWindows;
    }
}