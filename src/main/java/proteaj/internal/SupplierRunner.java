package proteaj.internal;

import java.util.function.Supplier;

/**
 * Created by ichikawa on 2017/07/14.
 */
public class SupplierRunner {
    public static <T> T run (Supplier<T> supplier) {
        return supplier.get();
    }
}
