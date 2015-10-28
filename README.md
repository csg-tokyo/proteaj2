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

Declaration of a protean operator
----

A declaration of a protean operator consists of its return type, syntax, parameter list, and operator body.
Let's see the following declaration.
 
      void "p" _ (String msg) { System.out.println(msg); }

This declaration defines a protean operator that returns `void` and has syntax `"p" _`.
The type of the operand is `String` and the corresponding parameter name is `msg`.
If we use this operator as follows, `System.out.println(msg)` is called and `Hello, world!` is displayed.
 
      p "Hello, world!"

Syntax of a protean operator is a sequence of operator names and operands.
An operator name is expressed by a double-quoted string like `"p"`.
An operand is expressed by an underscore symbol `_`.

A protean operator is not only infix, prefix, postfix, and outfix.
For example, we can define the following operators.
  
      int _ _ (int a, int b) { return a * b; }
      boolean _ "is" "instance" "of" _ (Object obj, Class<?> clazz) { return clazz.isInstance(obj); }

This feature is useful for implementing user-defined literals.


Author
----
Kazuhiro Ichikawa (ichikawa@csg.ci.i.u-tokyo.ac.jp)

License
----
Copyright (c) 2015 Kazuhiro Ichikawa  
This software is released under the MIT License.