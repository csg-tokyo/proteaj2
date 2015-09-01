package proteaj.lang;

import java.util.*;

public class ActivatedContexts {
  @SuppressWarnings("unchecked")
  public static <C> C get (int index) { return (C)(contexts.get()[index]); }

  public static void set (int index, Object context) {
    Object[] array = contexts.get();
    if (index < array.length) {
      array[index] = context;
    }
    else {
      array = Arrays.copyOf(array, Math.max(array.length * 2, index + 1));
      array[index] = context;
      contexts.set(array);
    }
  }

  private static ThreadLocal<Object[]> contexts = new ThreadLocal<Object[]>() {
    @Override
    protected Object[] initialValue() {
      return new Object[10];
    }
  };
}
