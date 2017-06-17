package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.option._

trait ExpectedTypeInferencer {
  this: JTypeLoader with IRExpressions with JModules with JMembers =>
  def inferExpectedType (param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure): Option[JType] = {
    inferExpectedType(param.genericType, binding, procedure)
  }

  def inferExpectedType(genType: JGenericType, binding: Map[String, MetaArgument], procedure: JProcedure): Option[JType] = {
    unbound(genType.unbound(binding).toList, binding, procedure).flatMap(genType.bind)
  }

  def inferContexts (cts: List[JGenericType], binding: Map[String, MetaArgument], procedure: JProcedure): Option[List[IRContextRef]] = contexts(cts, binding, procedure, Nil)

  private def contexts (cts: List[JGenericType], binding: Map[String, MetaArgument], procedure: JProcedure, cs: List[IRContextRef]): Option[List[IRContextRef]] = cts match {
    case ct :: rest => inferExpectedType(ct, binding, procedure) match {
      case Some(t: JObjectType) => contexts(rest, binding, procedure, IRContextRef(t) :: cs)
      case _                    => None
    }
    case Nil => Some(cs.reverse)
  }

  private def unbound (names: List[String], binding: Map[String, MetaArgument], procedure: JProcedure): Option[Map[String, MetaArgument]] = names match {
    case name :: rest => procedure.metaParameters.get(name).flatMap(mp => unboundMetaParameter(name, binding, mp.metaType, mp.bounds)) match {
      case Some(arg) => unbound(rest, binding + (name -> arg), procedure)
      case None      => None
    }
    case Nil => Some(binding)
  }

  private def unboundMetaParameter (name: String, binding: Map[String, MetaArgument], metaType: JTypeSignature, bounds: List[JTypeSignature]): Option[MetaArgument] = {
    if (metaType == JTypeSignature.typeTypeSig) bounds.traverse(fromTypeSignature_RefType(_, binding)).map(JUnboundTypeVariable(name, _))
    else fromTypeSignature_RefType(metaType, binding).map(JUnboundMetaVariable)
  }
}
