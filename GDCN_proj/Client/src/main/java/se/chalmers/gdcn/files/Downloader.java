package se.chalmers.gdcn.files;

import net.tomp2p.peers.PeerAddress;
import net.tomp2p.storage.Data;
import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.NetworkInterface;
import se.chalmers.gdcn.taskbuilder.communicationToClient.TaskFailureListener;
import se.chalmers.gdcn.taskbuilder.fileManagement.PathManager;
import sun.tools.jar.resources.jar;

import java.io.File;

/**
 * Created by HalfLeif on 2014-03-05.
 */
public class Downloader extends AbstractFileMaster {

    private final PeerAddress jobOwner;

    public Downloader(TaskMeta taskMeta, String projectName, NetworkInterface client, PeerAddress jobOwner,TaskFailureListener taskFailureListener) throws TaskMetaDataException {
        super(taskMeta, client, taskFailureListener, CommandWord.GET, PathManager.worker(projectName));

        this.jobOwner = jobOwner;
    }


    /**
     * {@inheritDoc}
     */
    @Override
    protected void ifFileExist(FileDep fileDep) {
        System.out.println("Found file: " + FileManagementUtils.pathTo(pathManager, fileDep));
        super.fileDependencyResolved(fileDep);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void ifFileDoNotExist(FileDep fileDep) {
        System.out.println("Didn't find file " + FileManagementUtils.pathTo(pathManager, fileDep));
        client.get(fileDep.getDhtKey(), jobOwner.getID());
        //Handling OperationFinished is done in AbstractFileMaster
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void operationForDependentFileSuccess(FileDep fileDep, Object result) {
        File file = FileManagementUtils.pathTo(pathManager, fileDep);
        Data data = (Data) result;
        FileManagementUtils.toFile(file, data.getData());
    }
}
