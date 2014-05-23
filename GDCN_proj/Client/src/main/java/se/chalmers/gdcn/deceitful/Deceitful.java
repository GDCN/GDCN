package se.chalmers.gdcn.deceitful;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

/**
 * Created by HalfLeif on 2014-05-23.
 *
 * Any method or class with this annotation is attempting to dupe or disrupt the jobOwner or network.
 */
@Target(ElementType.METHOD)
public @interface Deceitful {
}
