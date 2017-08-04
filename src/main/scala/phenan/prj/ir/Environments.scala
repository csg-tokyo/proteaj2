package phenan.prj.ir

import phenan.prj._

trait Environments {
  this: DSLEnvironments with FileEnvironments with NameResolvers with JTypeLoader with IRs with IRStatements with IRExpressions with JModules with JMembers =>

  trait Environment {
    def activateTypes: List[JRefType]

    def locals: Map[String, IRLocalVariableRef]

    def exceptions: List[JRefType]

    def dslEnvironment: DSLEnvironment

    def expressionOperators(expected: JType, priority: JPriority): List[ExpressionOperator] = dslEnvironment.expressionOperators(expected, priority)

    def literalOperators(expected: JType, priority: JPriority): List[LiteralOperator] = dslEnvironment.literalOperators(expected, priority)

    def inferContexts(procedure: JProcedure, bind: Map[String, MetaArgument]): Option[List[IRContextRef]] = dslEnvironment.inferContexts(procedure, bind)

    def localVariable(name: String): Option[IRLocalVariableRef] = locals.get(name)

    def defineLocal(localType: JType, name: String): Environment = Environment_LocalVariables(List((localType, name)), this)

    def activates (contexts: List[IRContextRef]): Environment = {
      if (contexts.nonEmpty) Environment_LocalContexts(contexts, Nil, this)
      else this
    }

    def deactivates (contexts: List[IRContextRef]): Environment = {
      val cs = dslEnvironment.contexts.filter(c => contexts.exists(_.contextType >:> c.contextType))
      if (cs.nonEmpty) Environment_LocalContexts(Nil, cs, this)
      else this
    }

    def defineLocals (locals: IRLocalDeclaration): Environment = Environment_LocalVariables(locals.declarators.map(local => (locals.localType.array(local.dim), local.name)), this)

    def inheritActiveContexts (from: Environment): Environment = {
      val diff = from.dslEnvironment.contexts.diff(this.dslEnvironment.contexts)
      if (diff.nonEmpty) Environment_LocalContexts(diff, Nil, this)
      else this
    }
  }

  trait BaseEnvironment {
    def declaringModule: IRModule
    def thisType: Option[JObjectType]
    def resolver: NameResolver
    def fileEnvironment: FileEnvironment

    lazy val highestPriority: Option[JPriority] = fileEnvironment.priorities.headOption
    def nextPriority(priority: JPriority): Option[JPriority] = nextPriorities.get(priority)
    private lazy val nextPriorities: Map[JPriority, JPriority] = fileEnvironment.priorities.zip(fileEnvironment.priorities.tail).toMap
  }

  sealed trait ModuleEnvironment extends BaseEnvironment with Environment {
    def procedureEnvironment(procedure: IRProcedure): ProcedureEnvironment = ProcedureEnvironment(procedure, this)

    def contexts: List[IRContextRef] = Nil

    def activateTypes = Nil

    def locals: Map[String, IRLocalVariableRef] = Map.empty

    def exceptions: List[JRefType] = Nil

    val dslEnvironment = DSLEnvironment(fileEnvironment.dsls, contexts)
  }

  class Environment_Instance(val declaringModule: IRModule, val fileEnvironment: FileEnvironment) extends ModuleEnvironment {
    def thisType: Option[JObjectType] = Some(declaringModule.thisType)

    def resolver: NameResolver = declaringModule.resolver
  }

  class Environment_Static(val declaringModule: IRModule, val fileEnvironment: FileEnvironment) extends ModuleEnvironment {
    def thisType: Option[JObjectType] = None

    def resolver: NameResolver = declaringModule.staticResolver
  }

  trait Environment_Variables extends Environment {
    def parent: Environment
    
    def variables: List[(JType, String)]

    val locals: Map[String, IRLocalVariableRef] = parent.locals ++ variables.map { case (t, n) => n -> IRLocalVariableRef(t, n) }
  }

  trait Environment_Contexts extends Environment {
    def parent: Environment

    def activated: List[IRContextRef]

    def deactivated: List[IRContextRef]

    val dslEnvironment: DSLEnvironment = parent.dslEnvironment.changeContext(activated, deactivated)
  }

  case class ProcedureEnvironment (procedure: IRProcedure, parent: ModuleEnvironment) extends Environment_Variables with Environment_Contexts with BaseEnvironment {
    def declaringModule: IRModule = parent.declaringModule

    def thisType: Option[JObjectType] = parent.thisType

    def fileEnvironment: FileEnvironment = parent.fileEnvironment

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