The ProteaJ compiler
====

ProteaJ is a Java-based language supporting "protean operators".
Protean operators include any operators that consist of a number of operator names and operands. 
Programmers can define protean operators and use them.

Simple examples
----

The following code defines a protean operator with syntax `"p" _`.

    dsl Print {
      void "p" _ (String msg) { System.out.println(msg); }
    }

We can use this operator as follows.

    import dsl Print;
    ...
    p "Hello, world!";


Author
----
Kazuhiro Ichikawa (ichikawa@csg.ci.i.u-tokyo.ac.jp)

License
----
Copyright (c) 2015 Kazuhiro Ichikawa  
This software is released under the MIT License.