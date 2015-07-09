package phenan.prj.ir

import phenan.prj._

import scalaz.Memo._

trait Environment {
  def clazz: JClass
  def thisType: Option[JObjectType]
  def contexts: List[IRContextRef]
  def locals: Map[String, IRLocalVariableRef]
  def fileEnvironment: FileEnvironment

  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator]
  def literalOperators (expected: JType, priority: JPriority): List[LiteralOperator]

  def inferContexts (procedure: JProcedure, bind: Map[String, MetaArgument]): Option[List[IRContextRef]]

  def resolver: NameResolver = fileEnvironment.file.resolver

  def highestPriority: Option[JPriority] = fileEnvironment.priorities.headOption
  def nextPriority (priority: JPriority): Option[JPriority] = nextPriorities.get(priority)

  def localVariable (name: String): Option[IRLocalVariableRef] = locals.get(name)

  def defineLocal (localType: JType, name: String): Environment = new Environment_Local(localType, name, this)
  def modifyContext (expression: IRExpression): Environment = new Environment_Context(expression.activates, expression.deactivates, this)

  def defineLocals (locals: IRLocalDeclaration): Environment = locals.declarators.foldLeft(this) { (e, d) =>
    e.defineLocal(locals.localType.array(d.dim), d.name)
  }

  private lazy val nextPriorities: Map[JPriority, JPriority] = fileEnvironment.priorities.zip(fileEnvironment.priorities.tail).toMap
}

class Environment_Local (localType: JType, name: String, parent: Environment) extends Environment {
  def clazz = parent.clazz
  def thisType = parent.thisType
  def contexts = parent.contexts
  val locals: Map[String, IRLocalVariableRef] = parent.locals + (name -> IRLocalVariableRef(localType, name))
  def fileEnvironment = parent.fileEnvironment
  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator] = parent.expressionOperators(expected, priority)
  def literalOperators (expected: JType, priority: JPriority): List[LiteralOperator] = parent.literalOperators(expected, priority)
  def inferContexts (procedure: JProcedure, bind: Map[String, MetaArgument]): Option[List[IRContextRef]] = parent.inferContexts(procedure, bind)
}

class Environment_Context (activates: List[IRContextRef], deactivates: List[IRContextRef], parent: Environment) extends Environment {
  def clazz = parent.clazz
  def thisType = parent.thisType
  def locals = parent.locals
  val contexts: List[IRContextRef] = activates ++ parent.contexts.diff(deactivates)
  def fileEnvironment = parent.fileEnvironment

  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator] = expressionOperators_cached(expected).getOrElse(priority, Nil)
  def literalOperators (expected: JType, priority: JPriority): List[LiteralOperator] = literalOperators_cached(expected).getOrElse(priority, Nil)

  def inferContexts (procedure: JProcedure, bind: Map[String, MetaArgument]): Option[List[IRContextRef]] = inferencer.inferContexts(procedure, bind, contexts).map(_._1)

  private val inferencer = new MethodContextInferencer(resolver.root.compiler.unifier)

  private val expressionOperators_cached: JType => Map[JPriority, List[ExpressionOperator]] = mutableHashMapMemo { t =>
    val fromDSL = fileEnvironment.dslExpressionOperators(t).flatMap {
      case (s, m, e) => inferencer.inferContexts(m, e, contexts).map {
        case (cs, e2) => ExpressionOperator(s, e2, m, { (ma, args) => IRDSLOperation(m, ma, args, cs) })
      }
    }
    val fromContexts = contexts.flatMap(c => collectOperators(c, t, c.contextType.expressionOperators)).flatMap {
      case (c, s, m, e) => inferencer.inferContexts(m, e, contexts).map {
        case (cs, e2) => ExpressionOperator(s, e2, m, { (ma, args) => IRContextOperation(c, m, ma, args, cs) })
      }
    }
    (fromDSL ++ fromContexts).groupBy(_.syntax.priority)
  }

  private val literalOperators_cached: JType => Map[JPriority, List[LiteralOperator]] = mutableHashMapMemo { t =>
    val fromDSL = fileEnvironment.dslLiteralOperators(t).flatMap {
      case (s, m, e) => inferencer.inferContexts(m, e, contexts).map {
        case (cs, e2) => LiteralOperator(s, e2, m, { (ma, args) => IRDSLOperation(m, ma, args, cs) })
      }
    }
    val fromContexts = contexts.flatMap(c => collectOperators(c, t, c.contextType.literalOperators)).flatMap {
      case (c, s, m, e) => inferencer.inferContexts(m, e, contexts).map {
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