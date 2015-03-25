package proteaj.lang;

import java.lang.annotation.*;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface OpElem {
  public OpElemType kind ();
  public String name() default "";
}
