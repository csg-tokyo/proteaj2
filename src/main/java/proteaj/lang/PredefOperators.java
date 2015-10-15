package proteaj.lang;

import proteaj.impl.*;

@DSL(
    priorities = { "mul", "add", "shift" },
    constraints = @Constraint({
        @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "mul"),
        @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add"),
        @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "shift")
    })
)
public class PredefOperators {
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
