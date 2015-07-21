package phenan.prj.ir

import phenan.prj._

import scalaz.Memo._

trait Environment {
  def clazz: IRModule
  def thisType: Option[JObjectType]
  def contexts: List[IRContextRef]
  def locals: Map[String, IRLocalVariableRef]
  def fileEnvironment: FileEnvironment
  def resolver: NameResolver

  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator]
  def literalOperators (expected: JType, priority: JPriority): List[LiteralOperator]

  def inferContexts (procedure: JProcedure, bind: Map[String, MetaArgument]): Option[List[IRContextRef]]

  def highestPriority: Option[JPriority] = fileEnvironment.priorities.headOption
  def nextPriority (priority: JPriority): Option[JPriority] = nextPriorities.get(priority)

  def localVariable (name: String): Option[IRLocalVariableRef] = locals.get(name)

  def defineLocal (localType: JType, name: String): Environment = Environment_LocalVariables(List((localType, name)), this)

  def modifyContext (statement: IRStatement): Environment = Environment_LocalContexts(statement.activates, statement.deactivates, this)
  def modifyContext (expression: IRExpression): Environment = Environment_LocalContexts(expression.activates, expression.deactivates, this)
  def withContexts (contexts: List[IRContextRef]): Environment = Environment_LocalContexts(contexts, Nil, this)

  def defineLocals (locals: IRLocalDeclaration): Environment = Environment_LocalVariables(locals.declarators.map(local => (locals.localType.array(local.dim), local.name)), this)

  private lazy val nextPriorities: Map[JPriority, JPriority] = fileEnvironment.priorities.zip(fileEnvironment.priorities.tail).toMap
}

sealed trait ModuleEnvironment extends Environment {
  def procedureEnvironment (procedure: IRProcedure): Environment = Environment_Method(procedure, this)

  def contexts: List[IRContextRef] = Nil
  def expressionOperators(expected: JType, priority: JPriority): List[ExpressionOperator] = fileEnvironment.expressionOperators(expected, priority)
  def literalOperators(expected: JType, priority: JPriority): List[LiteralOperator] = fileEnvironment.literalOperators(expected, priority)
  def inferContexts(procedure: JProcedure, bind: Map[String, MetaArgument]): Option[List[IRContextRef]] = inferencer.inferContexts(procedure, bind).map(_._1)
  def resolver = clazz.resolver
  private val inferencer = new MethodContextInferencer(clazz.compiler.unifier, Nil)
}

class Environment_Instance (val clazz: IRModule, val fileEnvironment: FileEnvironment) extends ModuleEnvironment {
  def thisType: Option[JObjectType] = clazz.thisType
  def locals: Map[String, IRLocalVariableRef] = Map.empty
}

class Environment_Static (val clazz: IRModule, val fileEnvironment: FileEnvironment) extends ModuleEnvironment {
  def thisType: Option[JObjectType] = None
  def locals: Map[String, IRLocalVariableRef] = Map.empty
}

sealed trait ChildEnvironment extends Environment {
  def parent: Environment

  def clazz = parent.clazz
  def thisType = parent.thisType
  def fileEnvironment = parent.fileEnvironment
}

trait Environment_Variables extends ChildEnvironment {
  def variables: List[(JType, String)]
  val locals: Map[String, IRLocalVariableRef] = parent.locals ++ variables.map { case (t, n) => n -> IRLocalVariableRef(t, n) }
}

trait Environment_Contexts extends ChildEnvironment {
  def activates: List[IRContextRef]
  def deactivates: List[IRContextRef]

  val contexts: List[IRContextRef] = activates ++ parent.contexts.diff(deactivates)

  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator] = expressionOperators_cached(expected).getOrElse(priority, Nil)
  def literalOperators (expected: JType, priority: JPriority): List[LiteralOperator] = literalOperators_cached(expected).getOrElse(priority, Nil)

  def inferContexts (procedure: JProcedure, bind: Map[String, MetaArgument]): Option[List[IRContextRef]] = inferencer.inferContexts(procedure, bind).map(_._1)

  private val inferencer = new MethodContextInferencer(resolver.root.compiler.unifier, contexts)

  private val expressionOperators_cached: JType => Map[JPriority, List[ExpressionOperator]] = mutableHashMapMemo { t =>
    val fromDSL = fileEnvironment.dslExpressionOperators(t).flatMap {
      case (s, m, e) => inferencer.inferContexts(m, e).map {
        case (cs, e2) => ExpressionOperator(s, e2, m, { (ma, args) => IRDSLOperation(m, ma, args, cs) })
      }
    }
    val fromContexts = contexts.flatMap(c => collectOperators(c, t, c.contextType.expressionOperators)).flatMap {
      case (c, s, m, e) => inferencer.inferContexts(m, e).map {
        case (cs, e2) => ExpressionOperator(s, e2, m, { (ma, args) => IRContextOperation(c, m, ma, args, cs) })
      }
    }
    (fromDSL ++ fromContexts).groupBy(_.syntax.priority)
  }

  private val literalOperators_cached: JType => Map[JPriority, List[LiteralOperator]] = mutableHashMapMemo { t =>
    val fromDSL = fileEnvironment.dslLiteralOperators(t).flatMap {
      case (s, m, e) => inferencer.inferContexts(m, e).map {
        case (cs, e2) => LiteralOperator(s, e2, m, { (ma, args) => IRDSLOperation(m, ma, args, cs) })
      }
    }
    val fromContexts = contexts.flatMap(c => collectOperators(c, t, c.contextType.literalOperators)).flatMap {
      case (c, s, m, e) => inferencer.inferContexts(m, e).map {
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

case class Environment_Method (procedure: IRProcedure, parent: Environment) extends Environment_Variables with Environment_Contexts {
  def variables: List[(JType, String)] = procedure.parameterVariables
  def activates: List[IRContextRef] = procedure.requiresContexts
  def deactivates: List[IRContextRef] = Nil
  def resolver: NameResolver = procedure.resolver
}

case class Environment_LocalVariables (variables: List[(JType, String)], parent: Environment) extends Environment_Variables {
  def contexts: List[IRContextRef] = parent.contexts
  def expressionOperators(expected: JType, priority: JPriority): List[ExpressionOperator] = parent.expressionOperators(expected, priority)
  def literalOperators(expected: JType, priority: JPriority): List[LiteralOperator] = parent.literalOperators(expected, priority)
  def inferContexts(procedure: JProcedure, bind: Map[String, MetaArgument]): Option[List[IRContextRef]] = parent.inferContexts(procedure, bind)
  def resolver: NameResolver = parent.resolver
}

case class Environment_LocalContexts (activates: List[IRContextRef], deactivates: List[IRContextRef], parent: Environment) extends Environment_Contexts {
  def locals: Map[String, IRLocalVariableRef] = parent.locals
  def resolver: NameResolver = parent.resolver
}

case class ExpressionOperator (syntax: JExpressionSyntax, metaArgs: Map[String, MetaArgument], method: JMethod, semantics: (Map[String, MetaArgument], List[IRExpression]) => IRExpression)
case class LiteralOperator (syntax: JLiteralSyntax, metaArgs: Map[String, MetaArgument], method: JMethod, semantics: (Map[String, MetaArgument], List[IRExpression]) => IRExpression)