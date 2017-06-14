package phenan.prj.ir

import phenan.prj._
import phenan.prj.declaration.QualifiedName

import scala.util._

case class FileEnvironment (file: IRFile) {
  def instanceEnvironment (clazz: IRModule): ModuleEnvironment = new Environment_Instance(clazz, this)
  def staticEnvironment (clazz: IRModule): ModuleEnvironment = new Environment_Static(clazz, this)

  lazy val dsls: List[JClassModule] = collectCompanions(collectDSLs(file.importedDSLNames), Set.empty).toList
  lazy val userConstraints: List[List[JPriority]] = file.userConstraints.map(resolver.constraint)
  lazy val priorities: List[JPriority] = sortPriorities(collectPriorities(dsls, Set.empty), dsls.flatMap(_.constraints) ++ userConstraints)

  def resolver: NameResolver = file.resolver

  private def collectCompanions (ds: Set[JClassModule], checked: Set[JClassModule]): Set[JClassModule] = {
    if (ds.isEmpty) checked
    else collectCompanions(ds.flatMap(_.withDSLs).intersect(checked), checked ++ ds)
  }

  private def collectDSLs (names: List[QualifiedName]): Set[JClassModule] = {
    collectDSLs(names, resolver.root.compiler.classLoader.predefOperatorsClass.map(_.classModule).toSet)
  }

  private def collectDSLs (names: List[QualifiedName], ds: Set[JClassModule]): Set[JClassModule] = names match {
    case n :: rest => resolver.resolve(n.names) match {
      case Success(d) =>
        collectDSLs(rest, ds + d.classModule)
      case Failure(e) =>
        file.state.error("invalid DSL name : " + n, e)
        collectDSLs(rest, ds)
    }
    case Nil => ds
  }

  private def collectPriorities (dsls: List[JClassModule], ps: Set[JPriority]): Set[JPriority] = dsls match {
    case m :: rest => collectPriorities(rest, ps ++ m.priorities)
    case Nil       => ps
  }

  private def sortPriorities (priorities: Set[JPriority], constraints: List[List[JPriority]]): List[JPriority] = {
    tsort(priorities.map(p => (p, constraints.flatMap(_.dropWhile(_ != p)).toSet - p)).toMap, Nil)
  }

  private def tsort (priorities: Map[JPriority, Set[JPriority]], sorted: List[JPriority]): List[JPriority] = priorities.find(_._2.isEmpty) match {
    case Some((p, _)) => tsort((priorities - p).mapValues(_ - p), p :: sorted)
    case None         =>
      if (priorities.nonEmpty) resolver.root.compiler.state.error("invalid priority : cyclic precedence")
      sorted.reverse
  }
}
