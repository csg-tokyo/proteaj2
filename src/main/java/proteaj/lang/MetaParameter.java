package proteaj.lang;

import java.lang.annotation.*;

@Target({ElementType.TYPE, ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface MetaParameter {
  public String name ();
  public String type() default "Lproteaj/lang/Type;";
  public String priority () default "";
  public String[] bounds() default {};
}
