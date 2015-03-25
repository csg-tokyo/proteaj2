package proteaj.lang;

import java.lang.annotation.*;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Operator {
  public OpLevel level () default OpLevel.Expression;
  public Association assoc () default Association.NON;
  public String priority () default "";
  public OpElem[] pattern ();
}
