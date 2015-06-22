package proteaj.lang;

import java.lang.annotation.*;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Operator {
  OpLevel level () default OpLevel.Expression;
  String priority ();
  OpElem[] pattern ();
}
