package phenan.prj.ir

import phenan.prj.decl._
import phenan.prj.internal.JClassLoader
import phenan.prj.state.JState

class IRFile (compilationUnit: CompilationUnit, loader: JClassLoader)(implicit state: JState) {
  val resolver = new IRResolver(compilationUnit.header, loader)

  val modules = compilationUnit.modules.map(moduleToIR)

  def getPackageName: Option[String] = resolver.packageName

  def getPackageInternalName: Option[String] = resolver.packageInternalName

  private def moduleToIR (module: ModuleDeclaration): IRModule = module match {
    case cls: ClassDeclaration      => new IRClass(cls, this)
    case enm: EnumDeclaration       => ???
    case ifc: InterfaceDeclaration  => ???
    case ann: AnnotationDeclaration => ???
    case dsl: DSLDeclaration        => ???
  }
}
