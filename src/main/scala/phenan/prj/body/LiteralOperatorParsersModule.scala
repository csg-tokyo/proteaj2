package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scalaz.Memo._

trait LiteralOperatorParsersModule {
  this: LiteralOperandParsersModule with CommonParsersModule with ContextSensitiveParsersModule
    with Unifier with Environments with DSLEnvironments with EnvModifyStrategy
    with IRs with IRExpressions with Syntax with JModules with JMembers with JErasedTypes with Application =>

  trait LiteralOperatorParsers {
    this: LiteralOperandParsers with CommonParsers with ContextSensitiveParsers =>

    def getLiteralOperatorParser(lop: LiteralOperator): ContextSensitiveScanner[IRExpression] = cached(lop).operator

    trait LiteralOperatorParsersInterface {
      def operator: ContextSensitiveScanner[IRExpression]
    }

    private val cached: LiteralOperator => LiteralOperatorParsersInterface = mutableHashMapMemo { new LiteralOperatorParsersImpl(_) }

    private class LiteralOperatorParsersImpl(lop: LiteralOperator) extends LiteralOperatorParsersInterface {
      lazy val operator: ContextSensitiveScanner[IRExpression] = constructParser(lop.syntax.pattern, lop.metaArgs, Nil)

      private def constructParser(pattern: List[JSyntaxElement], binding: Map[String, MetaArgument], operands: List[IRExpression]): ContextSensitiveScanner[IRExpression] = pattern match {
        case JOperand(param, p) :: rest => getLiteralOperandParser(param, p, binding, lop) >> { arg =>
          constructParser(rest, bind(param, arg, binding), arg :: operands)
        }
        case JOptionalOperand(param, p) :: rest =>
          (getLiteralOperandParser(param, p, binding, lop) |
            defaultArgument(param).fold(ContextSensitiveScanner.failure[IRExpression]("default argument is not found"))(ContextSensitiveScanner.success(_))) >> { arg =>
          constructParser(rest, bind(param, arg, binding), arg :: operands)
        }
        case JRepetition0(param, p) :: rest => rep0(param, p, binding, Nil) >> {
          case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
        }
        case JRepetition1(param, p) :: rest => rep1(param, p, binding, Nil) >> {
          case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
        }
        case JMetaOperand(name, param, p) :: rest =>
          if (binding.contains(name)) getMetaValueLiteralParser(name, binding(name), p, binding, lop) >> { bind => constructParser(rest, bind, operands) }
          else getMetaLiteralOperandParser(param, p, binding, lop) >> { ma => constructParser(rest, binding + (name -> ma), operands) }
        case JMetaName(name, value, p) :: rest => getMetaValueLiteralParser(name, value, p, binding, lop) >> { bind => constructParser(rest, bind, operands) }
        case JOperatorName(name) :: rest => word(name) ~> constructParser(rest, binding, operands)
        case JRegexName(name) :: rest => ( regex(name.r) ^^# IRStringLiteral ) >> { s => constructParser(rest, binding, s :: operands) }
        case JAndPredicate(param, p) :: rest => getLiteralOperandParser(param, p, binding, lop).& ~> constructParser(rest, binding, operands)
        case JNotPredicate(param, p) :: rest => getLiteralOperandParser(param, p, binding, lop).! ~> constructParser(rest, binding, operands)
        case Nil => ContextSensitiveScanner.success(lop.semantics(binding, operands.reverse))
      }

      private def rep0(param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], args: List[IRExpression]): ContextSensitiveScanner[(Map[String, MetaArgument], List[IRExpression])] = {
        rep1(param, pri, binding, args) | ContextSensitiveScanner.success((binding, args))
      }

      private def rep1(param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], args: List[IRExpression]): ContextSensitiveScanner[(Map[String, MetaArgument], List[IRExpression])] = {
        getLiteralOperandParser(param, pri, binding, lop) >> { arg => rep0(param, pri, bind(param, arg, binding), args :+ arg) }
      }

      private def bind(param: JParameter, arg: IRExpression, binding: Map[String, MetaArgument]) = arg.staticType match {
        case Some(t) => bindTypeArgs(param, t, binding)
        case None    => binding
      }

      private def defaultArgument (param: JParameter): Option[IRDefaultArgument] = {
        param.defaultArg.flatMap(name => lop.declaringClassModule.findMethod(name, declaringModule).find(_.erasedParameterTypes == Nil)).map(IRDefaultArgument)
      }
    }
  }
}