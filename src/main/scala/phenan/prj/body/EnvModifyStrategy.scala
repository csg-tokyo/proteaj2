package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

/**
  * Created by ichikawa on 2017/06/27.
  */
trait EnvModifyStrategy {
  this: ContextSensitiveParsersModule with Environments with IRStatements with IRExpressions with JModules =>

  implicit class EnvWithModify (env: Environment) {
    def modify [T: HasEnvModify] (t: T) : Environment = implicitly[HasEnvModify[T]].modify(env, t)
  }

  trait HasEnvModify [-T] {
    def modify (env: Environment, t: T): Environment
  }

  object HasEnvModify {
    implicit def listHasEnvModify[T : HasEnvModify]: HasEnvModify[List[T]] = (env, list) => list.foldLeft(env)(_.modify(_))
    implicit def sequenceHasEnvModify[T : HasEnvModify, U : HasEnvModify]: HasEnvModify[T ~ U] = (env, pair) => env.modify(pair._1).modify(pair._2)
    implicit def optionHasEnvModify[T : HasEnvModify]: HasEnvModify[Option[T]] = (env, opt) => opt.map(t => env.modify(t)).getOrElse(env)

    implicit val environmentHasEnvModify: HasEnvModify[Environment] = (env, _) => env
    implicit val unitHasEnvModify: HasEnvModify[Unit] = (env, _) => env

    implicit val metaArgumentHasEnvModify: HasEnvModify[MetaArgument] = (env, _) => env
    implicit val metaArgsHasEnvModify: HasEnvModify[MetaArgs] = (env, _) => env

    implicit val irExpressionHasEnvModify: HasEnvModify[IRExpression] = (env, expr) => expr.modifyEnv(env)
    implicit val irStatementHasEnvModify: HasEnvModify[IRStatement] = (env, stmt) => stmt.modifyEnv(env)

    implicit val irVariableDeclaratorHasEnvModify: HasEnvModify[IRVariableDeclarator] = (env, _) => env
    implicit val irLocalDeclarationHasEnvModify: HasEnvModify[IRLocalDeclaration] = (env, declaration) => env.defineLocals(declaration)
    implicit val irEnhancedForHeaderHasEnvModify: HasEnvModify[IREnhancedForHeader] = (env, header) => env.defineLocal(header.elementType.array(header.dim), header.name)
    implicit val irExceptionHandlerHeaderHasEnvModify: HasEnvModify[IRExceptionHandlerHeader] = (env, header) => env.defineLocal(header.exceptionType, header.name)
    implicit val irExceptionHandlerHasEnvModify: HasEnvModify[IRExceptionHandler] = (env, _) => env

    implicit val irExplicitConstructorCallHasEnvModify: HasEnvModify[IRExplicitConstructorCall] = (env, _) => env
    implicit val irMethodBodyHasEnvModify: HasEnvModify[IRMethodBody] = (env, _) => env
    implicit val irConstructorBodyHasEnvModify: HasEnvModify[IRConstructorBody] = (env, _) => env
    implicit val irInitializerBodyHasEnvModify: HasEnvModify[IRInitializerBody] = (env, _) => env

    implicit val parsedArgumentHasEnvModify: HasEnvModify[ParsedArgument] = (env, arg) => env.modify(arg._2)
    implicit val parsedArgumentListHasEnvModify: HasEnvModify[ParsedArgumentList] = (env, argsList) => env.modify(argsList._2)
  }
}
