package test;

import java.io.Serializable;
import java.util.*;

public class Foo {
  void foo () {
    hello.greet();
  }

  private Hello hello = new Hello();
}

class Hoge<A> {
  <T> List<? extends A> foo (T t, List<A> a) { return a; }
  A bar () { return null; }
}

class Box<T> {
  Box(T t) { this.t = t; }
  T get() { return t; }
  void put (T t) { this.t = t; }

  <U> Pair<T, U> mkPair(U u) { return new Pair<>(t, u); }
  T t;
}

class Pair<T, U> {
  Pair(T t, U u) { this.t = t; this.u = u; }
  T t;
  U u;
}

class Piyo {
  static void hoge() {
    Hoge<?> hoge = new Hoge<String>();
    //hoge.<String>foo("hoge", new ArrayList<String>());

    Hoge<?> fuga = new Hoge<List<String>>();

    hoge = fuga;

    Hoge<? extends String[]> a = null;
    Hoge<? super Serializable> b = null;
    //a = b;

    String[] array = a.bar();

//    List<List<? extends CharSequence>> l1 = mklistlist();
    List<? extends List<? extends CharSequence>> l2 = mklistlist();

//    CharSequence o = a.bar();

    Box<? super String> box = new Box<>("aaa");
    box.put("hoge");

    Box<? extends String> box2 = new Box<>("aaa");
    CharSequence s = box2.get();

    Pair<? extends CharSequence, String> pair = box2.mkPair("");
  }

  static <T> List<List<T>> mklistlist() { return null; }

  static <T extends List<String>, U extends T> void foo(T t) {
    List<? extends U> l2 = null;
    t = l2.get(0);
  }
}
