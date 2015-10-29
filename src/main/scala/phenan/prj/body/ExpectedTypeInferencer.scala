package phenan.prj.body

import phenan.prj._
import phenan.prj.ir.IRContextRef

import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.option._

object ExpectedTypeInferencer {
  def expected (param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure): Option[JType] = {
    expected(param.genericType, binding, procedure)
  }

  def expected (genType: JGenericType, binding: Map[String, MetaArgument], procedure: JProcedure): Option[JType] = {
    unbound(genType.unbound(binding).toList, binding, procedure).flatMap(genType.bind)
  }

  def contexts (cts: List[JGenericType], binding: Map[String, MetaArgument], procedure: JProcedure): Option[List[IRContextRef]] = contexts(cts, binding, procedure, Nil)

  private def contexts (cts: List[JGenericType], binding: Map[String, MetaArgument], procedure: JProcedure, cs: List[IRContextRef]): Option[List[IRContextRef]] = cts match {
    case ct :: rest => expected(ct, binding, procedure) match {
      case Some(t: JObjectType) => contexts(rest, binding, procedure, IRContextRef(t) :: cs)
      case _                    => None
    }
    case Nil => Some(cs.reverse)
  }

  private def unbound (names: List[String], binding: Map[String, MetaArgument], procedure: JProcedure): Option[Map[String, MetaArgument]] = names match {
    case name :: rest => procedure.metaParameters.get(name).flatMap(mp => unboundMetaParameter(name, binding, mp.metaType, mp.bounds, procedure.compiler)) match {
      case Some(arg) => unbound(rest, binding + (name -> arg), procedure)
      case None      => None
    }
    case Nil => Some(binding)
  }

  private def unboundMetaParameter (name: String, binding: Map[String, MetaArgument], metaType: JTypeSignature, bounds: List[JTypeSignature], compiler: JCompiler): Option[MetaArgument] = {
    if (metaType == JTypeSignature.typeTypeSig) bounds.traverse(compiler.typeLoader.fromTypeSignature_RefType(_, binding)).map(JUnboundTypeVariable(name, _, compiler))
    else compiler.typeLoader.fromTypeSignature_RefType(metaType, binding).map(JUnboundMetaVariable)
  }
}
