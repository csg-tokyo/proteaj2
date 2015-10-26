package phenan.prj.body

import phenan.prj._

object ExpectedTypeInferencer {
  def expected (param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure): Option[JType] = {
    unbound(param.genericType.unbound(binding).toList, binding, procedure).flatMap(param.genericType.bind)
  }

  private def unbound (names: List[String], binding: Map[String, MetaArgument], procedure: JProcedure): Option[Map[String, MetaArgument]] = names match {
    case name :: rest => procedure.metaParameters.get(name).flatMap(mp => unboundMetaParameter(name, binding, mp.metaType, mp.bounds, procedure.compiler)) match {
      case Some(arg) => unbound(rest, binding + (name -> arg), procedure)
      case None      => None
    }
    case Nil => Some(binding)
  }

  import scalaz.Scalaz._
  private def unboundMetaParameter (name: String, binding: Map[String, MetaArgument], metaType: JTypeSignature, bounds: List[JTypeSignature], compiler: JCompiler): Option[MetaArgument] = {
    if (metaType == JTypeSignature.typeTypeSig) bounds.traverse(compiler.typeLoader.fromTypeSignature_RefType(_, binding)).map(JUnboundTypeVariable(name, _, compiler))
    else compiler.typeLoader.fromTypeSignature_RefType(metaType, binding).map(MetaValueWildcard)
  }
}
