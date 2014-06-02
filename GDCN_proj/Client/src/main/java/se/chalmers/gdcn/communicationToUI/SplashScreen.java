package se.chalmers.gdcn.communicationToUI;

import se.chalmers.gdcn.taskbuilder.utils.FormatString;

/**
 * Created by joakim on 6/1/14.
 */
public class SplashScreen {

    /*private static final String SPLASH = "\n" +
            "   _____ _____   _____ _   _ \n" +
            "  / ____|  __ \\ / ____| \\ | |\n" +
            " | |  __| |  | | |    |  \\| |\n" +
            " | | |_ | |  | | |    | . ` |\n" +
            " | |__| | |__| | |____| |\\  |\n" +
            "  \\_____|_____/ \\_____|_| \\_|\n" +
            "\n" +
            "General Decentralized Computation Network\n";*/

    private static final String SPLASH = "\n" +
                "        GGGGGGGGGGGGG DDDDDDDDDDDDD                 CCCCCCCCCCCCC NNNNNNNN        NNNNNNNN\n" +
                "     GGG::::::::::::G D::::::::::::DDD           CCC::::::::::::C N:::::::N       N::::::N\n" +
                "   GG:::::::::::::::G D:::::::::::::::DD       CC:::::::::::::::C N::::::::N      N::::::N\n" +
                "  G:::::GGGGGGGG::::G DDD:::::DDDDD:::::D     C:::::CCCCCCCC::::C N:::::::::N     N::::::N\n" +
                " G:::::G       GGGGGG   D:::::D    D:::::D   C:::::C       CCCCCC N::::::::::N    N::::::N\n" +
                "G:::::G                 D:::::D     D:::::D C:::::C               N:::::::::::N   N::::::N\n" +
                "G:::::G                 D:::::D     D:::::D C:::::C               N:::::::N::::N  N::::::N\n" +
                "G:::::G    GGGGGGGGGG   D:::::D     D:::::D C:::::C               N::::::N N::::N N::::::N\n" +
                "G:::::G    G::::::::G   D:::::D     D:::::D C:::::C               N::::::N  N::::N:::::::N\n" +
                "G:::::G    GGGGG::::G   D:::::D     D:::::D C:::::C               N::::::N   N:::::::::::N\n" +
                "G:::::G        G::::G   D:::::D     D:::::D C:::::C               N::::::N    N::::::::::N\n" +
                " G:::::G       G::::G   D:::::D    D:::::D   C:::::C       CCCCCC N::::::N     N:::::::::N\n" +
                "  G:::::GGGGGGGG::::G DDD:::::DDDDD:::::D     C:::::CCCCCCCC::::C N::::::N      N::::::::N\n" +
                "   GG:::::::::::::::G D:::::::::::::::DD       CC:::::::::::::::C N::::::N       N:::::::N\n" +
                "     GGG::::::GGG:::G D::::::::::::DDD           CCC::::::::::::C N::::::N        N::::::N\n" +
                "        GGGGGG   GGGG DDDDDDDDDDDDD                 CCCCCCCCCCCCC NNNNNNNN         NNNNNNN\n" +
                "\n" +
                "                       General Decentralized Computation Network\n";

    public static void print() {
        System.out.println(FormatString.colour(SPLASH, FormatString.Colour.YELLOW));
    }
}
