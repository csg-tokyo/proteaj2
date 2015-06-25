package phenan.prj.ir

import phenan.prj._

trait Environment {
  def dsls: List[JClassModule]
  def contexts: List[JObjectType]
  def locals: Map[String, IRLocalVariableRef]

  def resolver: NameResolver

  def highestPriority: Option[JPriority] = priorities.headOption
  def nextPriority (priority: JPriority): Option[JPriority] = nextPriorities.get(priority)
  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator] = ???

  def localVariable (name: String): Option[IRLocalVariableRef] = locals.get(name)

  def defineLocal (localType: JType, name: String): Environment = new Environment_Local(localType, name, this)
  def modifyContext (expression: IRExpression): Environment = new Environment_Context(expression.activates, expression.deactivates, this)

  def defineLocals (locals: IRLocalDeclaration): Environment = locals.declarators.foldLeft(this) { (e, d) =>
    e.defineLocal(locals.localType.array(d.dim), d.name)
  }

  def priorities: List[JPriority] = sortPriorities(collectPriorities(dsls, Set.empty), dsls.flatMap(_.constraints))

  private lazy val nextPriorities: Map[JPriority, JPriority] = priorities.zip(priorities.tail).toMap

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

case class FileEnvironment (file: IRFile) extends Environment {
  def dsls: List[JClassModule] = ???

  def locals = Map.empty
  def contexts = Nil
  def resolver = file.resolver
}

class Environment_Local (localType: JType, name: String, parent: Environment) extends Environment {
  def dsls = parent.dsls
  val locals: Map[String, IRLocalVariableRef] = parent.locals + (name -> IRLocalVariableRef(localType, name))
  def contexts = parent.contexts
  def resolver = parent.resolver
}

class Environment_Context (activates: List[JObjectType], deactivates: List[JObjectType], parent: Environment) extends Environment {
  def dsls = parent.dsls
  def locals = parent.locals
  def contexts: List[JObjectType] = activates ++ parent.contexts.diff(deactivates)
  def resolver = parent.resolver
}

case class ExpressionOperator (syntax: JSyntax, semantics: List[IRArgument] => IRExpression)
