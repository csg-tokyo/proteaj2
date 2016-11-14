package test;

import proteaj.impl.*;

import static proteaj.impl.OpElemType.*;

@DSL(priorities = { "getter", "setter" }, constraints = { @Constraint({ @Priority(dsl = "Ltest/Var;", name = "getter"), @Priority(dsl = "Ltest/Var;", name = "setter") }) })
@ClassSig(metaParameters = { @MetaParameter(name = "T"), @MetaParameter(name = "id", type = "Lproteaj/lang/Identifier;") })
public class Var<T> {
  public Var (T v) {
    this.v = v;
  }

  @Operator(pattern = { @OpElem(kind = Reference, name = "id") }, priority = @Priority(dsl = "Ltest/Var;", name = "getter"))
  public T getter() {
    return v;
  }

  @Operator(pattern = { @OpElem(kind = Reference, name = "id"), @OpElem(kind = Name, name = "="), @OpElem(kind = Hole) }, priority = @Priority(dsl = "Ltest/Var;", name = "setter"))
  public void setter (T v) {
    this.v = v;
  }

  private T v;
}
