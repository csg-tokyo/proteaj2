package proteaj.lang;

import java.util.function.Function;

public abstract class ContextDependentBlock<C, R> implements Function<C, R> {
  abstract R block();

  @Override
  public R apply(C t) {
    // TODO: register context
    return block();
  }
}
