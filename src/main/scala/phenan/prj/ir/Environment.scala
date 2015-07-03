package phenan.prj.ir

import phenan.prj._
import phenan.prj.declaration.QualifiedName

import scala.util._
import scalaz.Memo._

trait Environment {
  def contexts: List[IRContextRef]
  def locals: Map[String, IRLocalVariableRef]
  def fileEnvironment: FileEnvironment

  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator]
  def literalOperators (expected: JType, priority: JPriority): List[LiteralOperator]

  def resolver: NameResolver = fileEnvironment.file.resolver

  def highestPriority: Option[JPriority] = fileEnvironment.priorities.headOption
  def nextPriority (priority: JPriority): Option[JPriority] = nextPriorities.get(priority)

  def localVariable (name: String): Option[IRLocalVariableRef] = locals.get(name)

  def defineLocal (localType: JType, name: String): Environment = new Environment_Local(localType, name, this)
  def modifyContext (expression: IRExpression): Environment = new Environment_Context(expression.activates, expression.deactivates, this)

  def defineLocals (locals: IRLocalDeclaration): Environment = locals.declarators.foldLeft(this) { (e, d) =>
    e.defineLocal(locals.localType.array(d.dim), d.name)
  }

  protected def inferContexts (method: JMethod, e: Map[String, MetaArgument]): Option[(List[IRContextRef], Map[String, MetaArgument])] = inferContexts(method.requires, Nil, e)

  private def inferContexts (requires: List[JGenericType], cs: List[IRContextRef], e: Map[String, MetaArgument]): Option[(List[IRContextRef], Map[String, MetaArgument])] = requires match {
    case req :: rest => findRequiredContext(req, e) match {
      case Some((c, e2)) => inferContexts(rest, c :: cs, e2)
      case None          => None
    }
    case Nil => Some((cs.reverse, e))
  }

  private def findRequiredContext (req: JGenericType, e: Map[String, MetaArgument]): Option[(IRContextRef, Map[String, MetaArgument])] = findRequiredContext(req, contexts, e)

  private def findRequiredContext (req: JGenericType, cs: List[IRContextRef], e: Map[String, MetaArgument]): Option[(IRContextRef, Map[String, MetaArgument])] = cs match {
    case c :: rest => resolver.root.compiler.unifier.infer(c.contextType, req, e) match {
      case Some(ma) => Some((c, ma))
      case None     => findRequiredContext(req, rest, e)
    }
    case Nil => None
  }

  private lazy val nextPriorities: Map[JPriority, JPriority] = fileEnvironment.priorities.zip(fileEnvironment.priorities.tail).toMap
}

case class FileEnvironment (file: IRFile) extends Environment {
  lazy val dsls: List[JClassModule] = collectDSLs(file.importedDSLNames, Nil)
  lazy val userConstraints: List[List[JPriority]] = file.userConstraints.map(resolver.constraint)
  lazy val priorities: List[JPriority] = sortPriorities(collectPriorities(dsls, Set.empty), dsls.flatMap(_.constraints) ++ userConstraints)

  def locals = Map.empty
  def contexts = Nil
  def fileEnvironment = this

  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator] = expressionOperators_cached(expected).getOrElse(priority, Nil)
  def literalOperators (expected: JType, priority: JPriority): List[LiteralOperator] = literalOperators_cached(expected).getOrElse(priority, Nil)

  val dslExpressionOperators: JType => List[(JExpressionSyntax, JMethod, Map[String, MetaArgument])] = mutableHashMapMemo { t => collectOperators(t, allExpressionOperators) }
  val dslLiteralOperators: JType => List[(JLiteralSyntax, JMethod, Map[String, MetaArgument])] = mutableHashMapMemo { t => collectOperators(t, allLiteralOperators) }

  private lazy val allExpressionOperators = dsls.flatMap(_.expressionOperators)
  private lazy val allLiteralOperators = dsls.flatMap(_.literalOperators)

  private val expressionOperators_cached: JType => Map[JPriority, List[ExpressionOperator]] = mutableHashMapMemo { t =>
    dslExpressionOperators(t).flatMap {
      case (s, m, e) => inferContexts(m, e).map {
        case (cs, e2) => ExpressionOperator(s, e2, m, { (ma, args) => IRDSLOperation(m, ma, args, cs) })
      }
    }.groupBy(_.syntax.priority)
  }

  private val literalOperators_cached: JType => Map[JPriority, List[LiteralOperator]] = mutableHashMapMemo { t =>
    dslLiteralOperators(t).flatMap {
      case (s, m, e) => inferContexts(m, e).map {
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

class Environment_Local (localType: JType, name: String, parent: Environment) extends Environment {
  val locals: Map[String, IRLocalVariableRef] = parent.locals + (name -> IRLocalVariableRef(localType, name))
  def contexts = parent.contexts
  def fileEnvironment = parent.fileEnvironment
  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator] = parent.expressionOperators(expected, priority)
  def literalOperators (expected: JType, priority: JPriority): List[LiteralOperator] = parent.literalOperators(expected, priority)
}

class Environment_Context (activates: List[IRContextRef], deactivates: List[IRContextRef], parent: Environment) extends Environment {
  def locals = parent.locals
  val contexts: List[IRContextRef] = activates ++ parent.contexts.diff(deactivates)
  def fileEnvironment = parent.fileEnvironment

  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator] = expressionOperators_cached(expected).getOrElse(priority, Nil)
  def literalOperators (expected: JType, priority: JPriority): List[LiteralOperator] = literalOperators_cached(expected).getOrElse(priority, Nil)

  private val expressionOperators_cached: JType => Map[JPriority, List[ExpressionOperator]] = mutableHashMapMemo { t =>
    val fromDSL = fileEnvironment.dslExpressionOperators(t).flatMap {
      case (s, m, e) => inferContexts(m, e).map {
        case (cs, e2) => ExpressionOperator(s, e2, m, { (ma, args) => IRDSLOperation(m, ma, args, cs) })
      }
    }
    val fromContexts = contexts.flatMap(c => collectOperators(c, t, c.contextType.expressionOperators)).flatMap {
      case (c, s, m, e) => inferContexts(m, e).map {
        case (cs, e2) => ExpressionOperator(s, e2, m, { (ma, args) => IRContextOperation(c, m, ma, args, cs) })
      }
    }
    (fromDSL ++ fromContexts).groupBy(_.syntax.priority)
  }

  private val literalOperators_cached: JType => Map[JPriority, List[LiteralOperator]] = mutableHashMapMemo { t =>
    val fromDSL = fileEnvironment.dslLiteralOperators(t).flatMap {
      case (s, m, e) => inferContexts(m, e).map {
        case (cs, e2) => LiteralOperator(s, e2, m, { (ma, args) => IRDSLOperation(m, ma, args, cs) })
      }
    }
    val fromContexts = contexts.flatMap(c => collectOperators(c, t, c.contextType.literalOperators)).flatMap {
      case (c, s, m, e) => inferContexts(m, e).map {
        case (cs, e2) => LiteralOperator(s, e2, m, { (ma, args) => IRContextOperation(c, m, ma, args, cs) })
      }
    }
    (fromDSL ++ fromContexts).groupBy(_.syntax.priority)
  }

  private def collectOperators [S <: JSyntax] (context: IRContextRef, expected: JType, operators: List[(S, JMethod)]): List[(IRContextRef, S, JMethod, Map[String, MetaArgument])] = collectOperators_helper (context, expected, operators, Nil)

  private def collectOperators_helper [S <: JSyntax] (context: IRContextRef, expected: JType, operators: List[(S, JMethod)], result: List[(IRContextRef, S, JMethod, Map[String, MetaArgument])]): List[(IRContextRef, S, JMethod, Map[String, MetaArgument])] = operators match {
    case (syntax, method) :: rest => resolver.root.compiler.unifier.unify(expected, method.returnType) match {
      case Some(ma) => collectOperators_helper (context, expected, rest, (context, syntax, method, ma) :: result)
      case None     => collectOperators_helper (context, expected, rest, result)
    }
    case Nil => result
  }
}

case class ExpressionOperator (syntax: JExpressionSyntax, metaArgs: Map[String, MetaArgument], method: JMethod, semantics: (Map[String, MetaArgument], List[IRExpression]) => IRExpression)
case class LiteralOperator (syntax: JLiteralSyntax, metaArgs: Map[String, MetaArgument], method: JMethod, semantics: (Map[String, MetaArgument], List[IRExpression]) => IRExpression)