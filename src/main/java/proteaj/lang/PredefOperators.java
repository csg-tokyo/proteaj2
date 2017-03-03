package proteaj.lang;

import proteaj.impl.*;

@DSL(
    priorities = { "mul", "add", "shift", "cmp", "eq" },
    constraints = @Constraint({
        @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "mul"),
        @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add"),
        @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "shift"),
        @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "cmp"),
        @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "eq")
    })
)
public class PredefOperators {
  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "eq"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "eq")),
          @OpElem(kind = OpElemType.Name, name = "=="),
          @OpElem(kind = OpElemType.Name, name = "null")
      }
  )
  public static boolean equalsToInt (Object obj) { return obj == null; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "eq"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "eq")),
          @OpElem(kind = OpElemType.Name, name = "!="),
          @OpElem(kind = OpElemType.Name, name = "null")
      }
  )
  public static boolean notEqualsToInt (Object obj) { return obj != null; }

  
  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "eq"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "eq")),
          @OpElem(kind = OpElemType.Name, name = "=="),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static boolean equalsToInt (int a, int b) { return a == b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "eq"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "eq")),
          @OpElem(kind = OpElemType.Name, name = "!="),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static boolean notEqualsToInt (int a, int b) { return a != b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "cmp"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "cmp")),
          @OpElem(kind = OpElemType.Name, name = "<"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static boolean lessThanInt (int a, int b) { return a < b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "cmp"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "cmp")),
          @OpElem(kind = OpElemType.Name, name = "<="),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static boolean lessThanOrEqualToInt (int a, int b) { return a <= b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "cmp"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "cmp")),
          @OpElem(kind = OpElemType.Name, name = ">"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static boolean greaterThanInt (int a, int b) { return a > b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "cmp"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "cmp")),
          @OpElem(kind = OpElemType.Name, name = ">="),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static boolean greaterThanOrEqualToInt (int a, int b) { return a >= b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "cmp"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "cmp")),
          @OpElem(kind = OpElemType.Name, name = "is"),
          @OpElem(kind = OpElemType.Name, name = "instance"),
          @OpElem(kind = OpElemType.Name, name = "of"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static boolean isInstanceOf (Object obj, Class<?> clazz) { return clazz.isInstance(obj); }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "shift"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "shift")),
          @OpElem(kind = OpElemType.Name, name = "<<"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static int shiftLInt (int a, int b) { return a << b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "shift"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "shift")),
          @OpElem(kind = OpElemType.Name, name = ">>"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static int shiftRInt (int a, int b) { return a >> b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "shift"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "shift")),
          @OpElem(kind = OpElemType.Name, name = ">>>"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static int shiftRUInt (int a, int b) { return a >>> b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add")),
          @OpElem(kind = OpElemType.Name, name = "+"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static String addString (String a, String b) { return a + b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add")),
          @OpElem(kind = OpElemType.Name, name = "+"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static int addInt (int a, int b) { return a + b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add")),
          @OpElem(kind = OpElemType.Name, name = "-"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static int subInt (int a, int b) { return a - b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "mul"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "mul")),
          @OpElem(kind = OpElemType.Name, name = "*"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static int mulInt (int a, int b) { return a * b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "mul"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "mul")),
          @OpElem(kind = OpElemType.Name, name = "/"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static int divInt (int a, int b) { return a / b; }

  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "mul"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "mul")),
          @OpElem(kind = OpElemType.Name, name = "%"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static int modInt (int a, int b) { return a % b; }
}
