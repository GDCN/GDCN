package se.chalmers.gdcn.files;

import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.peers.PeerMapChangeListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.Install;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;

import java.io.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by Niklas on 2014-04-01.
 */
public class NeighbourFileManager {

    //List containing the addresses which are in the neighbour file
    private Set<PeerAddress> fileNeighbours = new HashSet<>();

    private ArrayList<String[]> bootstrapNodes = new ArrayList<>();

    //The name of the neighbour file
    private String fileName;

    private String filePath;

    private String testDirectory;

    //The actual neighbour file
    private File neighbourFile;

    private File bootstrapNodeFile = new File(PathManager.getSettingsPath() + Install.BOOTSTRAP_NODE_NAME);

    //Listener used by PeerOwner to know when the addressmap changes in the Peer
    private final PeerMapChangeListener peerMapChangeListener = new PeerMapChangeListener() {
        @Override
        public void peerInserted(PeerAddress peerAddress) {

            Boolean bootstrap = false;

            for(String [] s: bootstrapNodes) {
                if(s[0].equals(peerAddress.getInetAddress().getHostAddress()) ||
                        s[0].equals(peerAddress.getInetAddress().getCanonicalHostName())) {
                    if(peerAddress.portTCP() == Integer.parseInt(s[1])) {
                        bootstrap = true;
                    }
                }
            }

            if(bootstrap) {
                return;
            }

            Boolean added = fileNeighbours.add(peerAddress);

            if(added) {
                writeNeighbours(peerAddress);
            }
        }

        @Override
        public void peerRemoved(PeerAddress peerAddress) {
            //DO NOTHING
        }

        @Override
        public void peerUpdated(PeerAddress peerAddress) {

            Boolean bootstrap = false;

            for(String [] s: bootstrapNodes) {
                if(s[0].equals(peerAddress.getInetAddress().getHostAddress()) ||
                        s[0].equals(peerAddress.getInetAddress().getCanonicalHostName())) {
                    if(peerAddress.portTCP() == Integer.parseInt(s[1])) {
                        bootstrap = true;
                    }
                }
            }

            if(bootstrap) {
                return;
            }


            fileNeighbours.remove(peerAddress);
            fileNeighbours.add(peerAddress);

            updateNeighbour(peerAddress);

        }
    };

    public NeighbourFileManager() {

        this("", "");

    }

    public NeighbourFileManager(String dir) {
        this(dir, "");
    }

    public NeighbourFileManager (String dir, String subpart) {

        testDirectory = PathManager.getSettingsPath() + File.separator + dir;

        filePath = testDirectory + File.separator + subpart;

        fileName = "neighbours";

        neighbourFile = new File(filePath+fileName);

//        fileNeighbours.clear();

        fileNeighbours.addAll(readNeighbours());

        bootstrapNodes = readBootstrapNodes();

    }

    private ArrayList<String[]> readBootstrapNodes() {

        ArrayList<String[]> bootNodes = new ArrayList<>();

        if(!bootstrapNodeFile.exists()) {
            return bootNodes;
        }

        String line;
        String[] address;

        try {
            BufferedReader in = new BufferedReader(new FileReader(bootstrapNodeFile));

            try {
                while((line = in.readLine()) != null) {

                    address = line.split(" ");

                    bootNodes.add(new String[]{address[0], address[1]});
                }
            } finally {
                in.close();
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        return bootNodes;
    }


    public PeerMapChangeListener getPeerMapListener() {
        return peerMapChangeListener;
    }

    public void writeNeighbours(PeerAddress peerAddress) {
        try {

            BufferedWriter out = new BufferedWriter(new FileWriter(neighbourFile, true));

            String output = "";

            output = output + (peerAddress.getID().toString() + " ");

            output = output + peerAddress.getInetAddress().getHostAddress() + " ";

            output = output + peerAddress.portTCP() + "\n";

            try {
                out.write(output);
            }

            finally {
                out.close();
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public Set<PeerAddress> readNeighbours(){
        Set<PeerAddress> fileNeigh = new HashSet<>();

        if(!neighbourFile.exists()) {
            return fileNeigh;
        }

        String line;
        String[] address;

        try {
            BufferedReader in = new BufferedReader(new FileReader(neighbourFile));

            try {
                while((line = in.readLine()) != null) {

                    address = line.split(" ");

                    fileNeigh.add(new PeerAddress(new Number160(address[0]), address[1],
                            Integer.parseInt(address[2]),Integer.parseInt(address[2])));
                }
            }
            finally {
                in.close();
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        return fileNeigh;
    }

    public void updateNeighbour(PeerAddress peerAddress) {


        String line;
        String[] address;

        String output = "";

        boolean found = false;

        try {
            BufferedReader in = new BufferedReader(new FileReader(neighbourFile));
            try {
                while((line = in.readLine()) != null) {

                    address = line.split(" ");

                    if(!found && new Number160(address[0]).equals(peerAddress.getID())) {
                        output = output + (peerAddress.getID().toString() + " ");

                        output = output + peerAddress.getInetAddress().getHostAddress() + " ";

                        output = output + peerAddress.portTCP() + "\n";

                        found = true;

                    } else {
                        output = line + "\n";
                    }
                }
            }

            finally {
                in.close();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        try {
            BufferedWriter out = new BufferedWriter(new FileWriter(neighbourFile));

            try {
                out.write(output);
            }

            finally {
                out.close();
            }

        } catch (IOException e) {
            e.printStackTrace();
        }


    }

    public HashSet<PeerAddress> getFileNeighbours() {
        return (HashSet<PeerAddress>) fileNeighbours;
    }

    public void clearNeighbourFile() {
        try {
            FileOutputStream writer = new FileOutputStream(neighbourFile);
            writer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void deleteNeighbourFile(){
        neighbourFile.delete();
    }

    public void changeNeighbourFileName(String fileName) {
        neighbourFile.renameTo(new File(fileName));

    }

    public ArrayList<String[]> getBootstrapNodes() {
        return bootstrapNodes;
    }
}
