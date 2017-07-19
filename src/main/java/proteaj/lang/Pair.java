package proteaj.lang;

/**
 * Created by ichikawa on 2017/07/07.
 */
public class Pair<A, B> {
    public Pair(A a, B b) { _1 = a; _2 = b; }
    public static <A, B> Pair<A, B> mkPair (A a, B b) { return new Pair<>(a, b); }
    public final A _1;
    public final B _2;
}
