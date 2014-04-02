package control;

import net.tomp2p.peers.Number160;
import net.tomp2p.peers.PeerAddress;
import net.tomp2p.peers.PeerMapChangeListener;
import taskbuilder.fileManagement.Install;
import taskbuilder.fileManagement.PathManager;

import java.io.*;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by Niklas on 2014-04-01.
 */
public class NeighbourManager {

    //List containing the addresses which are in the neighbour file
    private Set<PeerAddress> fileNeighbours = new HashSet<>();

    //The name of the neighbour file
    private String fileName = "neighbours";

    //The actual neighbour file
    private File neighbourFile;

    private PathManager pathManager;

    //Listener used by PeerOwner to know when the addressmap changes in the Peer
    private final PeerMapChangeListener peerMapChangeListener = new PeerMapChangeListener() {
        @Override
        public void peerInserted(PeerAddress peerAddress) {

            Boolean added = fileNeighbours.add(peerAddress);

            if(added) {
                System.out.println("peer is added " + peerAddress.getID());
                writeNeighbours(peerAddress);
            }
        }

        @Override
        public void peerRemoved(PeerAddress peerAddress) {
            //DO NOTHING
        }

        @Override
        public void peerUpdated(PeerAddress peerAddress) {

            fileNeighbours.remove(peerAddress);
            fileNeighbours.add(peerAddress);

            updateNeighbour(peerAddress);

        }
    };

    public NeighbourManager () {
        pathManager = PathManager.jobOwner("settings");

        PathManager.loadDefaultLocation();

        neighbourFile = new File(pathManager.getSettingsPath()+fileName);

        fileNeighbours.addAll(readNeighbours());

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

            out.write(output);

            out.close();

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
            while((line = in.readLine()) != null) {

                address = line.split(" ");

                fileNeigh.add(new PeerAddress(new Number160(address[0]), address[1],
                        Integer.parseInt(address[2]),Integer.parseInt(address[2])));
            }

            in.close();
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

            in.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        try {
            BufferedWriter out = new BufferedWriter(new FileWriter(neighbourFile));

            out.write(output);

            out.close();
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
}
