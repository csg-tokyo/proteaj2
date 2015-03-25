package proteaj.lang;

import java.lang.annotation.*;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface ClassSig {
  public GenericParameter[] genericParameters() default {};
  public String superType() default "Ljava/lang/Object;";
  public String[] interfaces() default {};
}
