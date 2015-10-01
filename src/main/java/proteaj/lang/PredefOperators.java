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
  @MethodSig(
      returnType = "I",
      parameters = { "I#proteaj/lang/PredefOperators.add", "I#proteaj/lang/PredefOperators.mul" }
  )
  @Operator(
      priority = @Priority(dsl = "Lproteaj/lang/PredefOperators;", name = "add"),
      pattern = {
          @OpElem(kind = OpElemType.Hole),
          @OpElem(kind = OpElemType.Name, name = "+"),
          @OpElem(kind = OpElemType.Hole)
      }
  )
  public static int addInt (int a, int b) {
    return a + b;
  }
}
