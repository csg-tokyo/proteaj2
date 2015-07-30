package proteaj.lang;

import java.util.*;

public class ActivatedContexts {
  @SuppressWarnings("unchecked")
  public static <C> C get (int index) { return (C)(contexts.get().get(index)); }

  public static void set (int index, Object context) { contexts.get().set(index, context); }

  private static ThreadLocal<List<Object>> contexts = new ThreadLocal<List<Object>>() {
    @Override
    protected List<Object> initialValue() {
      return new ArrayList<>();
    }
  };
}
