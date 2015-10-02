package proteaj.impl;

import java.lang.annotation.*;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface OpElem {
  OpElemType kind ();
  String name() default "";
  Priority[] priority() default {};
}
