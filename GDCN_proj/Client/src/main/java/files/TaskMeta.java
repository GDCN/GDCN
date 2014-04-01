package files;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Serialized data-class to Json
 *
 * Represents contents in one MetaTask file
 */
public class TaskMeta implements Serializable {
    private final String resultKey;
    private final String taskName;

    private final FileDep module;
    private final List<FileDep> dependencies;

    TaskMeta(String resultKey, String taskName, FileDep module, List<FileDep> dependencies) {
        this.resultKey = resultKey;
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
        if (!resultKey.equals(taskMeta.resultKey)) return false;
        if (!taskName.equals(taskMeta.taskName)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = resultKey.hashCode();
        result = 31 * result + taskName.hashCode();
        result = 31 * result + module.hashCode();
        return result;
    }

    public String getResultKey() {
        return resultKey;
    }

    public String getTaskName() {
        return taskName;
    }

    public FileDep getModule() {
        return module;
    }

    public List<FileDep> getDependencies() {
        return new ArrayList<>(dependencies);
    }
}
