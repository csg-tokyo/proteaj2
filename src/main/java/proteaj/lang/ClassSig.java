package proteaj.lang;

import java.lang.annotation.*;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface ClassSig {
  public MetaParameter[] metaParameters() default {};
  public String superType() default "Ljava/lang/Object;";
  public String[] interfaces() default {};
}
