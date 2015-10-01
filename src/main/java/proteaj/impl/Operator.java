package proteaj.impl;

import java.lang.annotation.*;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Operator {
  OpLevel level () default OpLevel.Expression;
  Priority priority ();
  OpElem[] pattern ();
}
