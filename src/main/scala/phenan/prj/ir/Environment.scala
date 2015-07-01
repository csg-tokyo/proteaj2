package phenan.prj.ir

import phenan.prj._
import phenan.prj.declaration.QualifiedName

import scala.util._
import scalaz.Memo._

trait Environment {
  def priorities: List[JPriority]
  def dsls: List[JClassModule]
  def contexts: List[IRContextRef]
  def locals: Map[String, IRLocalVariableRef]

  def resolver: NameResolver

  def highestPriority: Option[JPriority] = priorities.headOption
  def nextPriority (priority: JPriority): Option[JPriority] = nextPriorities.get(priority)
  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator] = getExpressionOperators(expected).getOrElse(priority, Nil)

  def localVariable (name: String): Option[IRLocalVariableRef] = locals.get(name)

  def defineLocal (localType: JType, name: String): Environment = new Environment_Local(localType, name, this)
  def modifyContext (expression: IRExpression): Environment = new Environment_Context(expression.activates, expression.deactivates, this)

  def defineLocals (locals: IRLocalDeclaration): Environment = locals.declarators.foldLeft(this) { (e, d) =>
    e.defineLocal(locals.localType.array(d.dim), d.name)
  }

  def getExpressionOperators: JType => Map[JPriority, List[ExpressionOperator]]

  private lazy val nextPriorities: Map[JPriority, JPriority] = priorities.zip(priorities.tail).toMap

  protected def collectDSLExpressions (t: JType, operators: List[(JExpressionSyntax, JMethod)], result: List[ExpressionOperator]): List[ExpressionOperator] = operators match {
    case (syntax, method) :: rest => unifier.unify(t, method.returnType) match {
      case Some(metaArgs) => collectDSLExpressions(t, rest, ExpressionOperator(syntax, metaArgs, method, { (metaArgs, args) => IRDSLOperation(method, metaArgs, args) }) :: result)
      case None           => collectDSLExpressions(t, rest, result)
    }
    case Nil => result
  }

  protected def collectContextExpressions (t: JType, context: IRContextRef, operators: List[(JExpressionSyntax, JMethod)], result: List[ExpressionOperator]): List[ExpressionOperator] = operators match {
    case (syntax, method) :: rest => unifier.unify(t, method.returnType) match {
      case Some(metaArgs) => collectContextExpressions(t, context, rest, ExpressionOperator(syntax, metaArgs, method, { (metaArgs, args) => IRContextOperation(context, method, metaArgs, args) }) :: result)
      case None           => collectContextExpressions(t, context, rest, result)
    }
    case Nil => result
  }

  private def unifier = resolver.root.compiler.unifier
}

case class FileEnvironment (file: IRFile) extends Environment {
  lazy val dsls: List[JClassModule] = collectDSLs(file.importedDSLNames, Nil)
  lazy val userConstraints: List[List[JPriority]] = file.userConstraints.map(resolver.constraint)

  def locals = Map.empty
  def contexts = Nil
  def resolver = file.resolver

  lazy val priorities: List[JPriority] = sortPriorities(collectPriorities(dsls, Set.empty), dsls.flatMap(_.constraints) ++ userConstraints)

  val getExpressionOperators: JType => Map[JPriority, List[ExpressionOperator]] = mutableHashMapMemo { t =>
    collectDSLExpressions(t, dsls.flatMap(_.expressionOperators), Nil).groupBy(_.syntax.priority)
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
  def dsls = parent.dsls
  def priorities = parent.priorities
  val locals: Map[String, IRLocalVariableRef] = parent.locals + (name -> IRLocalVariableRef(localType, name))
  def contexts = parent.contexts
  def resolver = parent.resolver

  def getExpressionOperators = parent.getExpressionOperators
}

class Environment_Context (activates: List[IRContextRef], deactivates: List[IRContextRef], parent: Environment) extends Environment {
  def dsls = parent.dsls
  def priorities = parent.priorities
  def locals = parent.locals
  val contexts: List[IRContextRef] = activates ++ parent.contexts.diff(deactivates)
  def resolver = parent.resolver

  val getExpressionOperators: JType => Map[JPriority, List[ExpressionOperator]] = mutableHashMapMemo { t =>
    (collectDSLExpressions(t, dsls.flatMap(_.expressionOperators), Nil) ++ contexts.flatMap(c => collectContextExpressions(t, c, c.contextType.expressionOperators, Nil))).groupBy(_.syntax.priority)
  }
}

case class ExpressionOperator (syntax: JExpressionSyntax, metaArgs: Map[String, MetaValue], method: JMethod, semantics: (Map[String, MetaValue], List[IRExpression]) => IRExpression)
