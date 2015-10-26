package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

import scalaz.Memo._

trait LiteralOperatorParsers {
  this: LiteralOperandParsers with CommonParsers with TwoLevelParsers =>

  def getLiteralOperatorParser (lop: LiteralOperator, env: Environment): LParser[IRExpression] = cached((lop, env)).operator

  trait LiteralOperatorParsersInterface {
    def operator: LParser[IRExpression]
  }

  private val cached: ((LiteralOperator, Environment)) => LiteralOperatorParsersInterface = mutableHashMapMemo { pair => new LiteralOperatorParsersImpl(pair._1, pair._2) }

  private class LiteralOperatorParsersImpl (lop: LiteralOperator, env: Environment) extends LiteralOperatorParsersInterface {
    lazy val operator: LParser[IRExpression] = constructParser(lop.syntax.pattern, lop.metaArgs, Nil)

    private def constructParser (pattern: List[JSyntaxElement], binding: Map[String, MetaArgument], operands: List[IRExpression]): LParser[IRExpression] = pattern match {
      case JOperand(param, p) :: rest           => getLiteralOperandParser(param, p, binding, lop.method, env) >> { arg =>
        constructParser(rest, bind(param, arg, binding), arg :: operands)
      }
      case JOptionalOperand(param, p) :: rest   => getLiteralOperandParser(param, p, binding, lop.method, env).?.mapOption { _.orElse(defaultArgument(param, lop.method, env)) } >> { arg =>
        constructParser(rest, bind(param, arg, binding), arg :: operands)
      }
      case JRepetition0(param, p) :: rest       => rep0(param, p, binding, Nil) >> {
        case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
      }
      case JRepetition1(param, p) :: rest       => rep1(param, p, binding, Nil) >> {
        case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
      }
      case JMetaOperand(name, param, p) :: rest => getLiteralOperandParser(param, p, binding, lop.method, env) >> {
        ast => constructParser(rest, binding + (name -> ConcreteMetaValue(ast, param)), operands)
      }
      case JMetaName(value, p) :: rest          => metaValue(value, p, binding) ~> constructParser(rest, binding, operands)
      case JOperatorName(name) :: rest          => word(name) ~> constructParser(rest, binding, operands)
      case JRegexName(name) :: rest             => regex(name) >> { s => constructParser(rest, binding, IRStringLiteral(s, compiler) :: operands) }
      case JAndPredicate(param, p) :: rest      => getLiteralOperandParser(param, p, binding, lop.method, env).& ~> constructParser(rest, binding, operands)
      case JNotPredicate(param, p) :: rest      => getLiteralOperandParser(param, p, binding, lop.method, env).! ~> constructParser(rest, binding, operands)
      case Nil                                  => LParser.success(lop.semantics(binding, operands.reverse))
    }

    private def metaValue (mv: MetaArgument, pri: Option[JPriority], binding: Map[String, MetaArgument]): LParser[MetaArgument] = mv match {
      case c: ConcreteMetaValue                            => getLiteralOperandParser(c.parameter, pri, binding, lop.method, env) ^? { case v if c.ast == v => c }
      case _: JRefType | _: JWildcard | _: MetaVariableRef => LParser.failure("type name cannot be used in a literal")
      case _: MetaValueWildcard                            => LParser.failure("meta value wildcard cannot be placed in operator pattern")
    }

    private def rep0 (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], args: List[IRExpression]): LParser[(Map[String, MetaArgument], List[IRExpression])] = {
      rep1(param, pri, binding, args) | LParser.success((binding, args))
    }

    private def rep1 (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], args: List[IRExpression]): LParser[(Map[String, MetaArgument], List[IRExpression])] = {
      getLiteralOperandParser(param, pri, binding, lop.method, env) >> { arg => rep0(param, pri, bind(param, arg, binding), args :+ arg) }
    }

    private def bind (param: JParameter, arg: IRExpression, binding: Map[String, MetaArgument]) = arg.staticType match {
      case Some(t) => compiler.unifier.bind(param, t, binding)
      case None    => binding
    }

    private def defaultArgument (param: JParameter, procedure: JProcedure, environment: Environment) = for {
      name   <- param.defaultArg
      method <- procedure.declaringClass.classModule.findMethod(name, environment.clazz).find(_.erasedParameterTypes == Nil)
    } yield IRDefaultArgument(method)

    private def compiler = env.clazz.compiler
  }
}
