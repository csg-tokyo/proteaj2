package phenan.prj

import scalaz.Memo._

class OperatorPool (compiler: JCompiler) {
  def dslExpressionOperators (dsl: JClassModule, expected: JType) = dslExpressionOperatorsMemo((dsl, expected))
  def dslLiteralOperators (dsl: JClassModule, expected: JType) = dslLiteralOperatorsMemo((dsl, expected))
  def contextExpressionOperators (context: JObjectType, expected: JType) = contextExpressionOperatorsMemo((context, expected))
  def contextLiteralOperators (context: JObjectType, expected: JType) = contextLiteralOperatorsMemo((context, expected))

  private val dslExpressionOperatorsMemo: ((JClassModule, JType)) => List[(JExpressionSyntax, JMethod, Map[String, MetaArgument])] = mutableHashMapMemo {
    case (dsl, expected) => collectOperators(expected, dsl.expressionOperators)
  }

  private val dslLiteralOperatorsMemo: ((JClassModule, JType)) => List[(JLiteralSyntax, JMethod, Map[String, MetaArgument])] = mutableHashMapMemo {
    case (dsl, expected) => collectOperators(expected, dsl.literalOperators)
  }

  private val contextExpressionOperatorsMemo: ((JObjectType, JType)) => List[(JExpressionSyntax, JMethod, Map[String, MetaArgument])] = mutableHashMapMemo {
    case (context, expected) => collectOperators(expected, context.expressionOperators)
  }

  private val contextLiteralOperatorsMemo: ((JObjectType, JType)) => List[(JLiteralSyntax, JMethod, Map[String, MetaArgument])] = mutableHashMapMemo {
    case (context, expected) => collectOperators(expected, context.literalOperators)
  }

  private def collectOperators [S <: JSyntax] (expected: JType, operators: List[(S, JMethod)]): List[(S, JMethod, Map[String, MetaArgument])] = collectOperators_helper (expected, operators, Nil)

  private def collectOperators_helper [S <: JSyntax] (expected: JType, operators: List[(S, JMethod)], result: List[(S, JMethod, Map[String, MetaArgument])]): List[(S, JMethod, Map[String, MetaArgument])] = operators match {
    case (syntax, method) :: rest => compiler.unifier.unify(expected, method.returnType) match {
      case Some(ma) if checkBounds(expected, ma, method.returnBounds) => collectOperators_helper (expected, rest, (syntax, method, ma) :: result)
      case _ => collectOperators_helper (expected, rest, result)
    }
    case Nil => result
  }

  private def checkBounds (expected: JType, metaArgs: Map[String, MetaArgument], bounds: List[JGenericType]): Boolean = bounds match {
    case bound :: rest => bound.bind(metaArgs) match {
      case Some(b) if expected <:< b => checkBounds(expected, metaArgs, rest)
      case _ => false
    }
    case Nil => true
  }
}
