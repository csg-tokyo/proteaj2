package phenan.prj.generator.sir2javarepr

import phenan.prj.generator.SimplifiedIRs
import phenan.prj.generator.JavaRepr._
import phenan.util._

/**
  * Created by ichikawa on 2017/07/14.
  */
trait JavaStatementGeneratorsModule {
  this: JavaExpressionGeneratorsModule with JavaSignatureGeneratorsModule with SimplifiedIRs =>

  object JavaStatementGenerators {
    import JavaSignatureGenerators.typeToSig
    import JavaSignatureGenerators.metaArgument
    import JavaExpressionGenerators.expression

    def block(block: SIRBlock): Block = Block(block.statements.map(statement))

    def statement (stmt: SIRStatement): Statement = stmt match {
      case b: SIRBlock =>
        Union[Statement](block(b))
      case SIRLocalDeclaration(t, name, ini) =>
        Union[Statement](LocalDeclaration(typeToSig(t), name, ini.map(expression)))
      case SIRIfStatement(cond, thenStmt, elseStmt) =>
        Union[Statement](IfStatement(expression(cond), statement(thenStmt), elseStmt.map(statement)))
      case SIREnhancedForStatement(elemType, name, iter, body) =>
        Union[Statement](EnhancedForStatement(typeToSig(elemType), name, expression(iter), statement(body)))
      case SIRWhileStatement(cond, body) =>
        Union[Statement](WhileStatement(expression(cond), statement(body)))
      case SIRTryStatement(tryBlock, catchBlocks, finallyBlock) =>
        Union[Statement](TryStatement(block(tryBlock), catchBlocks.map(exceptionHandler), finallyBlock.map(block)))
      case SIRThrowStatement(exception) =>
        Union[Statement](ThrowStatement(expression(exception)))
      case SIRReturnStatement(value) =>
        Union[Statement](ReturnStatement(value.map(expression)))
      case SIRExpressionStatement(expr) =>
        Union[Statement](ExpressionStatement(expression(expr)))
      case SIRBreakStatement =>
        Union[Statement](BreakStatement)
      case SIRThisConstructorCall(metaArgs, args) =>
        Union[Statement](ThisConstructorCall(metaArgs.flatMap(metaArgument), args.map(expression)))
      case SIRSuperConstructorCall(metaArgs, args) =>
        Union[Statement](SuperConstructorCall(metaArgs.flatMap(metaArgument), args.map(expression)))
    }

    def exceptionHandler (handler: SIRExceptionHandler): ExceptionHandler = {
      ExceptionHandler(typeToSig(handler.exceptionType), handler.name, block(handler.catchBlock))
    }
  }
}
