package proteaj.impl;

import java.lang.annotation.*;

@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface MethodSig {
  MetaParameter[] metaParameters() default {};
  String returnType();
  String[] parameters();
  String[] throwsTypes() default {};
  String[] activates() default {};
  String[] deactivates() default {};
  String[] requires() default {};
}
