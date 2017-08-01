package phenan.prj.ir

import phenan.prj._

trait IRStatements {
  this: Environments with IRExpressions with JModules =>

  case class IRMethodBody(block: IRBlock)

  case class IRConstructorBody(constructorCall: Option[IRExplicitConstructorCall], statements: List[IRStatement])

  case class IRInitializerBody(block: IRBlock)

  sealed trait IRStatement {
    def modifyEnv (env: Environment): Environment
  }

  case class IRBlock(statements: List[IRStatement]) extends IRStatement {
    override def modifyEnv(env: Environment): Environment = env.inheritActiveContexts(statements.foldLeft(env)((e, s) => s.modifyEnv(e)))
  }

  case class IRLocalDeclarationStatement(declaration: IRLocalDeclaration) extends IRStatement {
    override def toString: String = declaration.toString + ';'
    override def modifyEnv(env: Environment): Environment = env.defineLocals(declaration)
  }

  case class IRLocalDeclaration(localType: JType, declarators: List[IRVariableDeclarator]) {
    override def toString: String = localType.name + ' ' + declarators.mkString(",")
  }

  case class IRVariableDeclarator(name: String, dim: Int, init: Option[IRExpression]) {
    private def typeName = name + (0 until dim).map(_ => "[]").mkString

    override def toString: String = typeName + init.map(e => " " + e.toString).getOrElse("")
  }

  case class IRIfStatement(condition: IRExpression, thenStatement: IRStatement, elseStatement: Option[IRStatement]) extends IRStatement {
    override def modifyEnv(env: Environment): Environment = env
  }

  case class IRWhileStatement(condition: IRExpression, statement: IRStatement) extends IRStatement {
    override def modifyEnv(env: Environment): Environment = env
  }

  sealed trait IRForStatement extends IRStatement {
    override def modifyEnv(env: Environment): Environment = env
  }

  case class IRNormalForStatement(local: IRLocalDeclaration, condition: Option[IRExpression], update: List[IRExpression], statement: IRStatement) extends IRForStatement

  case class IRAncientForStatement(init: List[IRExpression], condition: Option[IRExpression], update: List[IRExpression], statement: IRStatement) extends IRForStatement

  case class IREnhancedForStatement(header: IREnhancedForHeader, statement: IRStatement) extends IRForStatement

  case class IREnhancedForHeader(elementType: JType, name: String, dim: Int, iterable: IRExpression)

  case class IRTryStatement(tryBlock: IRBlock, catchBlocks: List[IRExceptionHandler], finallyBlock: Option[IRBlock]) extends IRStatement {
    override def modifyEnv(env: Environment): Environment = env
  }

  case class IRExceptionHandler (header: IRExceptionHandlerHeader, catchBlock: IRBlock)

  case class IRExceptionHandlerHeader (exceptionType: JType, name: String)

  case class IRActivateStatement(expression: IRExpression) extends IRStatement {
    override def modifyEnv(env: Environment): Environment = env
  }

  case class IRThrowStatement(expression: IRExpression) extends IRStatement {
    override def modifyEnv(env: Environment): Environment = env
  }

  case class IRReturnStatement(expression: IRExpression) extends IRStatement {
    override def modifyEnv(env: Environment): Environment = env
  }

  case class IRExpressionStatement(expression: IRExpression) extends IRStatement {
    override def modifyEnv(env: Environment): Environment = expression.modifyEnv(env)
  }

}