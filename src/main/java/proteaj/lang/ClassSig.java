package proteaj.lang;

import java.lang.annotation.*;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface ClassSig {
  MetaParameter[] metaParameters() default {};
  String superType() default "Ljava/lang/Object;";
  String[] interfaces() default {};
}
