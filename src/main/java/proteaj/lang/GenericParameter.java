package proteaj.lang;

import java.lang.annotation.*;

@Target({ElementType.TYPE, ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface GenericParameter {
  public String name ();
  public String parameterType() default "Lproteaj/lang/Type;";
  public String[] bounds() default {};
}
