package test;

import proteaj.lang.*;
import static proteaj.lang.OpElemType.*;

@Context
@DSL(priorities = { "test.Var.getter", "test.Var.setter" })
@ClassSig(metaParameters = { @MetaParameter(name = "T"), @MetaParameter(name = "id", type = "Lproteaj/lang/Identifier;") })
public class Var<T> {
  public Var (T v) {
    this.v = v;
  }

  @Operator(pattern = { @OpElem(kind = Reference, name = "id") }, priority = "test.Var.getter")
  public T getter() {
    return v;
  }

  @Operator(pattern = { @OpElem(kind = Reference, name = "id"), @OpElem(kind = Name, name = "="), @OpElem(kind = Hole) }, priority = "test.Var.setter")
  public void setter (T v) {
    this.v = v;
  }

  private T v;
}
