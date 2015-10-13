package phenan.prj.ir

import phenan.prj._

import scalaz.Memo._

class DSLEnvironment (dsls: List[JClassModule], contexts: List[IRContextRef], compiler: JCompiler) {
  def expressionOperators (expected: JType, priority: JPriority): List[ExpressionOperator] = expressionOperatorsMap(expected).getOrElse(priority, Nil)
  def literalOperators (expected: JType, priority: JPriority): List[LiteralOperator] = literalOperatorsMap(expected).getOrElse(priority, Nil)

  def inferContexts (procedure: JProcedure, bind: Map[String, MetaArgument]): Option[List[IRContextRef]] = inferencer.inferContexts(procedure, bind).map(_._1)

  private val inferencer = new MethodContextInferencer(compiler.unifier, contexts)

  private val expressionOperatorsMap: JType => Map[JPriority, List[ExpressionOperator]] = mutableHashMapMemo { expected =>
    val fromDSL = dsls.flatMap { dsl =>
      compiler.operatorPool.dslExpressionOperators(dsl, expected).flatMap {
        case (s, m, e) => inferencer.inferContexts(m, e).map {
          case (cs, e2) => ExpressionOperator(s, e2, m, { (ma, args) => IRDSLOperation(m, ma, args, cs) })
        }
      }
    }
    val fromContexts = contexts.flatMap { context =>
      compiler.operatorPool.contextExpressionOperators(context.contextType, expected).flatMap {
        case (s, m, e) => inferencer.inferContexts(m, e).map {
          case (cs, e2) => ExpressionOperator(s, e2, m, { (ma, args) => IRContextOperation(context, m, ma, args, cs) })
        }
      }
    }
    (fromDSL ++ fromContexts).groupBy(_.syntax.priority)
  }

  private val literalOperatorsMap: JType => Map[JPriority, List[LiteralOperator]] = mutableHashMapMemo { expected =>
    val fromDSL = dsls.flatMap { dsl =>
      compiler.operatorPool.dslLiteralOperators(dsl, expected).flatMap {
        case (s, m, e) => inferencer.inferContexts(m, e).map {
          case (cs, e2) => LiteralOperator(s, e2, m, { (ma, args) => IRDSLOperation(m, ma, args, cs) })
        }
      }
    }
    val fromContexts = contexts.flatMap { context =>
      compiler.operatorPool.contextLiteralOperators(context.contextType, expected).flatMap {
        case (s, m, e) => inferencer.inferContexts(m, e).map {
          case (cs, e2) => LiteralOperator(s, e2, m, { (ma, args) => IRContextOperation(context, m, ma, args, cs) })
        }
      }
    }
    (fromDSL ++ fromContexts).groupBy(_.syntax.priority)
  }
}

case class ExpressionOperator (syntax: JExpressionSyntax, metaArgs: Map[String, MetaArgument], method: JMethod, semantics: (Map[String, MetaArgument], List[IRExpression]) => IRExpression)
case class LiteralOperator (syntax: JLiteralSyntax, metaArgs: Map[String, MetaArgument], method: JMethod, semantics: (Map[String, MetaArgument], List[IRExpression]) => IRExpression)
