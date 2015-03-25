package test;

import proteaj.lang.*;
import static proteaj.lang.OpElemType.*;

@Context
@ClassSig(genericParameters = { @GenericParameter(name = "T"), @GenericParameter(name = "id", parameterType = "Lproteaj/lang/Identifier;") })
public class Var<T> {
  public Var (T v) {
    this.v = v;
  }

  @Operator(pattern = { @OpElem(kind = Reference, name = "id") })
  public T getter() {
    return v;
  }

  @Operator(pattern = { @OpElem(kind = Reference, name = "id"), @OpElem(kind = Name, name = "="), @OpElem(kind = Hole) })
  public void setter (T v) {
    this.v = v;
  }

  private T v;
}
