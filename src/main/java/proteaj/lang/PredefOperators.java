package proteaj.lang;

import proteaj.impl.*;

@DSL(
    priorities = { "mul", "add" },
    constraints = @Constraint({
        @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "mul"),
        @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add")
    })
)
public class PredefOperators {
  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add"),
      pattern = {
          @OpElem(kind = OpElemType.Hole, priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add")),
          @OpElem(kind = OpElemType.Name, name = "+"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static int addInt (int a, int b) {
    return a + b;
  }
}
