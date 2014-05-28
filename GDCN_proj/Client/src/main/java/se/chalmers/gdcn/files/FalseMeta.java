package se.chalmers.gdcn.files;

import net.tomp2p.peers.Number160;

/**
 * Created by HalfLeif on 2014-05-28.
 */
public class FalseMeta extends TaskMeta {

    private final FileDep falseModule;

    /**
     * Creates a deceitful TaskMeta
     * @param taskMeta ordinary task meta to imitate
     * @param falseModuleFileName file name for modified file
     */
    public static FalseMeta falsify(TaskMeta taskMeta, String falseModuleFileName){
        return new FalseMeta(taskMeta, falseModuleFileName);
    }

    /**
     * Creates a deceitful TaskMeta
     * @param taskMeta ordinary task meta to imitate
     * @param falseModuleFileName file name for modified file
     */
    private FalseMeta(TaskMeta taskMeta, String falseModuleFileName) {
        super(taskMeta.getTaskName(), taskMeta.getModule(), taskMeta.getDependencies());
        FileDep module = taskMeta.getModule();
        this.falseModule = new FalseDep(falseModuleFileName, module.getFileLocation(), module.getDhtKey(),
                module.isSticky(), module.getCheckSum());
    }

    private static class FalseDep extends FileDep{

        private FalseDep(String fileName, String fileLocation, Number160 dhtKey, boolean sticky, int checkSum) {
            super(fileName, fileLocation, dhtKey, sticky, checkSum);
        }
    }

    @Override
    public FileDep getModule(){
        return falseModule;
    }

}
