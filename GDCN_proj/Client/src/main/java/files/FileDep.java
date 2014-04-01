package files;

import java.io.Serializable;

/**
 * Serialized data-class to Json
 *
 * Represent one file that has to be resolved for Task to compile or run
 */
class FileDep implements Serializable {
    private String fileName;
    private String location;
    private String key;

    private boolean sticky = false;
    //TODO put checksum elsewhere
    private int checkSum;

    FileDep(String fileName, String location, String key, boolean sticky, int checkSum) {
        this.fileName = fileName;
        this.location = location;
        this.key = key;
        this.sticky = sticky;
        this.checkSum = checkSum;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof FileDep)) return false;

        FileDep fileDep = (FileDep) o;

        if (!fileName.equals(fileDep.fileName)) return false;
        if (!key.equals(fileDep.key)) return false;
        if (!location.equals(fileDep.location)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = fileName.hashCode();
        result = 31 * result + location.hashCode();
        result = 31 * result + key.hashCode();
        return result;
    }

    public String getFileName() {
        return fileName;
    }

    public String getLocation() {
        return location;
    }

    public String getKey() {
        return key;
    }

    public boolean isSticky() {
        return sticky;
    }

    public int getCheckSum() {
        return checkSum;
    }
}
