package phenan.prj.ir

import phenan.prj._

trait Environments {
  this: DSLEnvironments with FileEnvironments with NameResolvers with JTypeLoader with IRs with IRStatements with IRExpressions with JModules with JMembers =>

  trait Environment {
    def clazz: IRModule

    def thisType: Option[JObjectType]

    def activateTypes: List[JRefType]

    def locals: Map[String, IRLocalVariableRef]

    def exceptions: List[JRefType]

    def fileEnvironment: FileEnvironment

    def dslEnvironment: DSLEnvironment

    def expressionOperators(expected: JType, priority: JPriority): List[ExpressionOperator] = dslEnvironment.expressionOperators(expected, priority)

    def literalOperators(expected: JType, priority: JPriority): List[LiteralOperator] = dslEnvironment.literalOperators(expected, priority)

    def inferContexts(procedure: JProcedure, bind: Map[String, MetaArgument]): Option[List[IRContextRef]] = dslEnvironment.inferContexts(procedure, bind)

    def highestPriority: Option[JPriority] = fileEnvironment.priorities.headOption

    def nextPriority(priority: JPriority): Option[JPriority] = nextPriorities.get(priority)

    def localVariable(name: String): Option[IRLocalVariableRef] = locals.get(name)

    def defineLocal(localType: JType, name: String): Environment = Environment_LocalVariables(List((localType, name)), this)

    def modifyContext (expression: IRExpression): Environment = {
      if (expression.activates.nonEmpty || expression.deactivates.nonEmpty) Environment_LocalContexts(expression.activates, expression.deactivates, this)
      else this
    }

    def withContexts (activates: List[IRContextRef], deactivates: List[IRContextRef]): Environment = {
      if (activates.nonEmpty || deactivates.nonEmpty) Environment_LocalContexts(activates, deactivates, this)
      else this
    }

    def defineLocals(locals: IRLocalDeclaration): Environment = Environment_LocalVariables(locals.declarators.map(local => (locals.localType.array(local.dim), local.name)), this)

    private lazy val nextPriorities: Map[JPriority, JPriority] = fileEnvironment.priorities.zip(fileEnvironment.priorities.tail).toMap
  }

  sealed trait ModuleEnvironment extends Environment {
    def procedureEnvironment(procedure: IRProcedure): ProcedureEnvironment = ProcedureEnvironment(procedure, this)

    def contexts: List[IRContextRef] = Nil

    def activateTypes = Nil

    def locals: Map[String, IRLocalVariableRef] = Map.empty

    def exceptions: List[JRefType] = Nil

    val dslEnvironment = new DSLEnvironment(fileEnvironment.dsls, contexts)

    def resolver: NameResolver
  }

  class Environment_Instance(val clazz: IRModule, val fileEnvironment: FileEnvironment) extends ModuleEnvironment {
    def thisType: Option[JObjectType] = clazz.thisType

    def resolver: NameResolver = clazz.resolver
  }

  class Environment_Static(val clazz: IRModule, val fileEnvironment: FileEnvironment) extends ModuleEnvironment {
    def thisType: Option[JObjectType] = None

    def resolver: NameResolver = clazz.staticResolver
  }

  sealed trait ChildEnvironment extends Environment {
    def parent: Environment

    def clazz: IRModule = parent.clazz

    def thisType: Option[JObjectType] = parent.thisType

    def fileEnvironment: FileEnvironment = parent.fileEnvironment
  }

  trait Environment_Variables extends ChildEnvironment {
    def variables: List[(JType, String)]

    val locals: Map[String, IRLocalVariableRef] = parent.locals ++ variables.map { case (t, n) => n -> IRLocalVariableRef(t, n) }
  }

  trait Environment_Contexts extends ChildEnvironment {
    def activated: List[IRContextRef]

    def deactivated: List[IRContextRef]

    val dslEnvironment: DSLEnvironment = parent.dslEnvironment.changeContext(activated, deactivated)
  }

  case class ProcedureEnvironment (procedure: IRProcedure, parent: Environment) extends Environment_Variables with Environment_Contexts {
    def variables: List[(JType, String)] = procedure.parameterVariables

    def activated: List[IRContextRef] = procedure.requiresContexts

    def deactivated: List[IRContextRef] = Nil

    def activateTypes: List[JRefType] = procedure.activateTypes

    def exceptions: List[JRefType] = procedure.exceptions ++ uncheckedExceptionTypes

    def resolver: NameResolver = procedure.resolver
  }

  case class Environment_LocalVariables(variables: List[(JType, String)], parent: Environment) extends Environment_Variables {
    def dslEnvironment: DSLEnvironment = parent.dslEnvironment

    def activateTypes: List[JRefType] = parent.activateTypes

    def exceptions: List[JRefType] = parent.exceptions
  }

  case class Environment_LocalContexts(activated: List[IRContextRef], deactivated: List[IRContextRef], parent: Environment) extends Environment_Contexts {
    def locals: Map[String, IRLocalVariableRef] = parent.locals

    def activateTypes: List[JRefType] = parent.activateTypes

    def exceptions: List[JRefType] = parent.exceptions
  }
}