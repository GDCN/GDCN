package se.chalmers.gdcn.files;

import net.tomp2p.peers.Number160;

import java.io.Serializable;

/**
 * Serialized data-class to Json
 *
 * Represent one file that has to be resolved for Task to compile or run
 */
//TODO Visibility changed for convenience for QualityControl, may exist better solution
public class FileDep implements Serializable {

    final private String fileName;
    final private String fileLocation;
    private Number160 dhtKey;

    //TODO generate dhtKey from projectName + fileName? will be hashed later on to a Number160

    private boolean sticky = false;
    //TODO put checksum elsewhere
    final private int checkSum;

    FileDep(String fileName, String fileLocation, Number160 dhtKey, boolean sticky, int checkSum) {
        this.fileName = fileName;
        this.fileLocation = fileLocation;
        this.dhtKey = dhtKey;
        this.sticky = sticky;
        this.checkSum = checkSum;
    }

    //TODO Not used, remove?
    FileDep(String fileName, String fileLocation, String dhtKey, boolean sticky, int checkSum) {
        this.fileName = fileName;
        this.fileLocation = fileLocation;
        this.dhtKey = Number160.createHash(dhtKey);
        this.sticky = sticky;
        this.checkSum = checkSum;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof FileDep)) return false;

        FileDep fileDep = (FileDep) o;

        if (!fileName.equals(fileDep.fileName)) return false;
        if (!dhtKey.equals(fileDep.dhtKey)) return false;
        if (!fileLocation.equals(fileDep.fileLocation)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = fileName.hashCode();
        result = 31 * result + fileLocation.hashCode();
        result = 31 * result + dhtKey.hashCode();
        return result;
    }

    public String getFileName() {
        return fileName;
    }

    public String getFileLocation() {
        return fileLocation;
    }

    public void setDhtKey(Number160 dhtKey) {
        this.dhtKey = dhtKey;
    }

    public Number160 getDhtKey() {
        return dhtKey;
    }

    public boolean isSticky() {
        return sticky;
    }

    public int getCheckSum() {
        return checkSum;
    }
}
