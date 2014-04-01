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
    public int hashCode(){
        return key.hashCode();
    }

    @Override
    public boolean equals(Object o){
        if(o == null){
            return false;
        }
        if(!(o instanceof FileDep)){
            return false;
        }
        return key.equals(((FileDep) o).key);
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
