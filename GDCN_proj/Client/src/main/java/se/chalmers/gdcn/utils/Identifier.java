package se.chalmers.gdcn.utils;

import java.io.Serializable;

/**
 * Created by Leif on 2014-04-16.
 *
 * More or less a typedef for String. Useful for type checking. Extend to make new ID type.
 */
public abstract class Identifier implements Serializable{
    private final String id;

    public Identifier(String id) {
        this.id = id;
    }

    @Override
    public String toString() {
        return id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Identifier)) return false;

        Identifier that = (Identifier) o;

        if (!id.equals(that.id)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return id.hashCode();
    }
}
