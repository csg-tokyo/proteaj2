package phenan.prj.ir

import phenan.prj._
import phenan.prj.declaration.QualifiedName

import scala.util._
import scalaz.Memo._

case class FileEnvironment (file: IRFile) {
  lazy val dsls: List[JClassModule] = collectDSLs(file.importedDSLNames, Nil)
  lazy val userConstraints: List[List[JPriority]] = file.userConstraints.map(resolver.constraint)
  lazy val priorities: List[JPriority] = sortPriorities(collectPriorities(dsls, Set.empty), dsls.flatMap(_.constraints) ++ userConstraints)

  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator] = expressionOperators_cached(expected).getOrElse(priority, Nil)
  def literalOperators (expected: JType, priority: JPriority): List[LiteralOperator] = literalOperators_cached(expected).getOrElse(priority, Nil)

  val dslExpressionOperators: JType => List[(JExpressionSyntax, JMethod, Map[String, MetaArgument])] = mutableHashMapMemo { t => collectOperators(t, allExpressionOperators) }
  val dslLiteralOperators: JType => List[(JLiteralSyntax, JMethod, Map[String, MetaArgument])] = mutableHashMapMemo { t => collectOperators(t, allLiteralOperators) }

  def resolver = file.resolver

  private lazy val allExpressionOperators = dsls.flatMap(_.expressionOperators)
  private lazy val allLiteralOperators = dsls.flatMap(_.literalOperators)

  private val inferencer = new MethodContextInferencer(resolver.root.compiler.unifier)

  private val expressionOperators_cached: JType => Map[JPriority, List[ExpressionOperator]] = mutableHashMapMemo { t =>
    dslExpressionOperators(t).flatMap {
      case (s, m, e) => inferencer.inferContexts(m, e, Nil).map {
        case (cs, e2) => ExpressionOperator(s, e2, m, { (ma, args) => IRDSLOperation(m, ma, args, cs) })
      }
    }.groupBy(_.syntax.priority)
  }

  private val literalOperators_cached: JType => Map[JPriority, List[LiteralOperator]] = mutableHashMapMemo { t =>
    dslLiteralOperators(t).flatMap {
      case (s, m, e) => inferencer.inferContexts(m, e, Nil).map {
        case (cs, e2) => LiteralOperator(s, e2, m, { (ma, args) => IRDSLOperation(m, ma, args, cs) })
      }
    }.groupBy(_.syntax.priority)
  }

  private def collectOperators [S <: JSyntax] (expected: JType, operators: List[(S, JMethod)]): List[(S, JMethod, Map[String, MetaArgument])] = collectOperators_helper (expected, operators, Nil)

  private def collectOperators_helper [S <: JSyntax] (expected: JType, operators: List[(S, JMethod)], result: List[(S, JMethod, Map[String, MetaArgument])]): List[(S, JMethod, Map[String, MetaArgument])] = operators match {
    case (syntax, method) :: rest => resolver.root.compiler.unifier.unify(expected, method.returnType) match {
      case Some(ma) => collectOperators_helper (expected, rest, (syntax, method, ma) :: result)
      case None     => collectOperators_helper (expected, rest, result)
    }
    case Nil => result
  }

  private def collectDSLs (names: List[QualifiedName], ds: List[JClassModule]): List[JClassModule] = names match {
    case n :: rest => resolver.resolve(n.names) match {
      case Success(d) => collectDSLs(rest, d.classModule :: ds)
      case Failure(e) =>
        file.state.error("invalid DSL name : " + n, e)
        collectDSLs(rest, ds)
    }
    case Nil => ds
  }

  private def collectPriorities (dsls: List[JClassModule], ps: Set[JPriority]): Set[JPriority] = dsls match {
    case m :: rest => collectPriorities(rest, ps ++ m.priorities)
    case Nil       => ps
  }

  private def sortPriorities (priorities: Set[JPriority], constraints: List[List[JPriority]]): List[JPriority] = {
    tsort(priorities.map(p => (p, constraints.flatMap(_.dropWhile(_ != p)).toSet - p)).toMap, Nil)
  }

  private def tsort (priorities: Map[JPriority, Set[JPriority]], sorted: List[JPriority]): List[JPriority] = priorities.find(_._2.isEmpty) match {
    case Some((p, _)) => tsort((priorities - p).mapValues(_ - p), p :: sorted)
    case None         =>
      if (priorities.nonEmpty) resolver.root.compiler.state.error("invalid priority : cyclic precedence")
      sorted.reverse
  }
}
