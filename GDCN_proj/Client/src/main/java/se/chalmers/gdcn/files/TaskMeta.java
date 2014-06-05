package se.chalmers.gdcn.files;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Serialized data-class to Json
 *
 * Represents contents in one MetaTask file
 */
public class TaskMeta implements Serializable {
    private final String taskName;

    private final FileDep module;
    private final List<FileDep> dependencies;

    TaskMeta(String taskName, FileDep module, List<FileDep> dependencies) {
        this.taskName = taskName;
        this.module = module;
        this.dependencies = new ArrayList<>(dependencies);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TaskMeta)) return false;

        TaskMeta taskMeta = (TaskMeta) o;

        if (!module.equals(taskMeta.module)) return false;
        if (!taskName.equals(taskMeta.taskName)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = taskName.hashCode();
        result = 31 * result + module.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "Meta{"+taskName+"}";
    }

    public String getTaskName() {
        return taskName;
    }

    public FileDep getModule() {
        return module;
    }

    //TODO Visibility changed for convenience for QualityControl, may exist better solution
    public List<FileDep> getDependencies() {
        return new ArrayList<>(dependencies);
    }
}
