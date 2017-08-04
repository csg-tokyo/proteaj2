package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

trait LiteralOperandParsersModule {
  this: LiteralParsersModule with CommonParsersModule with ContextSensitiveParsersModule
    with ExpectedTypeInferencer with DSLEnvironments with EnvModifyStrategy
    with IRExpressions with JModules with JMembers =>

  trait LiteralOperandParsers {
    this: LiteralParsers with CommonParsers with ContextSensitiveParsers =>

    def getLiteralOperandParser(param: JParameter, pri: Option[JPriority], binding: MetaArgs, lop: LiteralOperator): ContextSensitiveScanner[ParsedArgument] = {
      for {
        expectedType <- inferExpectedType(param, binding, lop.method)
        contexts     <- inferContexts(param.contexts, binding, lop.method)
        withoutCts   <- inferContexts(param.withoutCts, binding, lop.method)
      } yield getLiteralParser(expectedType, pri, lop.syntax.priority).withLocalContexts(contexts).withoutLocalContexts(withoutCts).argumentFor(param, binding)
    }.getOrElse(ContextSensitiveScanner.failure[ParsedArgument]("fail to parse argument expression"))

    def getMetaLiteralOperandParser(param: JParameter, pri: Option[JPriority], binding: MetaArgs, lop: LiteralOperator): ContextSensitiveScanner[MetaArgument] = {
      inferExpectedType(param, binding, lop.method) match {
        case Some(e) => getMetaArgumentParser(e, pri, lop)
        case None => ContextSensitiveScanner.failure[MetaArgument]("fail to parse meta expression")
      }
    }

    def getMetaValueLiteralParser(name: String, mv: MetaArgument, pri: Option[JPriority], binding: MetaArgs, lop: LiteralOperator): ContextSensitiveScanner[MetaArgs] = mv match {
      case c: ConcreteMetaValue => getMetaArgumentParser(c.valueType, pri, lop) ^? { case v if c.ast == v => c } ^^^ binding
      case u: JUnboundMetaVariable => getMetaArgumentParser(u.valueType, pri, lop) ^^ { arg => binding + (name -> arg) }
      case _: JRefType | _: JWildcard | _: MetaVariableRef => ContextSensitiveScanner.failure[MetaArgs]("type name cannot be used in a literal")
    }

    private def getMetaArgumentParser(metaType: JType, pri: Option[JPriority], lop: LiteralOperator): ContextSensitiveScanner[MetaArgument] = {
      getLiteralParser(metaType, pri, lop.syntax.priority) ^^ { arg => ConcreteMetaValue(arg, metaType) }
    }
  }
}