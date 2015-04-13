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
  T get() { return null; }
  void put (T t) {}
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

    Box<? super String> box = new Box<>();
    box.put("hoge");


  }

  static <T> List<List<String>> mklistlist() { return null; }
}
