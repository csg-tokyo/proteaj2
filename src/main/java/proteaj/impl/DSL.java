package proteaj.impl;

import java.lang.annotation.*;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface DSL {
  String[] priorities() default {};
  Constraint[] constraints() default {};
  Class<?>[] with() default {};
}
