package proteaj.impl;

import java.lang.annotation.*;

@Target({ElementType.TYPE, ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface MetaParameter {
  String name ();
  String type() default "Lproteaj/lang/Type;";
  String[] bounds() default {};
}
