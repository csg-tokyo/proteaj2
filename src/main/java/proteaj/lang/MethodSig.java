package proteaj.lang;

import java.lang.annotation.*;

@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface MethodSig {
  public GenericParameter[] genericParameters() default {};
  public String returnType();
  public String[] parameterTypes();
  public String[] throwsTypes() default {};
  public String[] activates() default {};
  public String[] deactivates() default {};
  public String[] requires() default {};
}
