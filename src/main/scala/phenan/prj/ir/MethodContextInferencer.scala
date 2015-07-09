package phenan.prj.ir

import phenan.prj._
import phenan.prj.typing._

class MethodContextInferencer (unifier: Unifier) {
  def inferContexts (procedure: JProcedure, e: Map[String, MetaArgument], contexts: List[IRContextRef]): Option[(List[IRContextRef], Map[String, MetaArgument])] = inferContexts(procedure.requires, Nil, e, contexts)

  private def inferContexts (requires: List[JGenericType], cs: List[IRContextRef], e: Map[String, MetaArgument], contexts: List[IRContextRef]): Option[(List[IRContextRef], Map[String, MetaArgument])] = requires match {
    case req :: rest => findRequiredContext(req, contexts, e) match {
      case Some((c, e2)) => inferContexts(rest, c :: cs, e2, contexts)
      case None          => None
    }
    case Nil => Some((cs.reverse, e))
  }

  private def findRequiredContext (req: JGenericType, cs: List[IRContextRef], e: Map[String, MetaArgument]): Option[(IRContextRef, Map[String, MetaArgument])] = cs match {
    case c :: rest => unifier.infer(c.contextType, req, e) match {
      case Some(ma) => Some((c, ma))
      case None     => findRequiredContext(req, rest, e)
    }
    case Nil => None
  }
}
