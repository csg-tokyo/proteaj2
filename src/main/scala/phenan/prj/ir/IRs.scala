package phenan.prj.ir

import phenan.prj._
import phenan.prj.JCompiler
import phenan.prj.declaration._
import phenan.prj.state.JState
import phenan.util._

import JModifier._

case class IRFile (ast: CompilationUnit, root: RootResolver)(implicit val state: JState) {
  lazy val modules: List[IRClass] = collectModules(ast.modules.map(IRClass(_, this)), Nil)

  lazy val internalName = ast.header.pack.map(_.name.names.mkString("/"))

  lazy val resolver = root.file(this)

  private def collectModules (modules: List[IRClass], result: List[IRClass]): List[IRClass] = modules match {
    case m :: ms => collectModules(ms ++ m.inners, result :+ m)
    case Nil     => result
  }

  def compiler = root.compiler
}

trait IRClass extends JClass {
  def ast: ModuleDeclaration
  def file: IRFile

  def outer: Option[IRClass]
  def inners: List[IRClass]

  private[ir] def implicitMethodModifier: Int

  def simpleName = ast.name
  lazy val name = internalName.replace('/', '.').replace('$', '.')

  lazy val outerClass: Option[String] = outer.map(_.internalName)
  
  lazy val internalName = outerClass match {
    case Some(outer) => outer + '$' + simpleName
    case None        => file.internalName.map(_ + '/').getOrElse("") + simpleName
  }

  lazy val innerClasses = inners.map(m => m.simpleName -> m.internalName).toMap
  
  lazy val resolver = file.resolver.inClass(this)
  
  def compiler: JCompiler = file.compiler
  def state: JState = file.state
}

object IRClass {
  def apply (module: ModuleDeclaration, file: IRFile): IRClass = apply(module, None, file)
  def apply (module: ModuleDeclaration, outer: Option[IRClass], file: IRFile): IRClass = module match {
    case c: ClassDeclaration => IRClassDef(c, outer, file)
    case _ => ???
  }
}

case class IRClassDef (ast: ClassDeclaration, outer: Option[IRClass], file: IRFile) extends IRClass {
  lazy val mod: JModifier = IRModifiers.mod(ast.modifiers) | accSuper

  lazy val inners: List[IRClass] = ast.members.collect {
    case m: ModuleDeclaration => IRClass(m, Some(this), file)
  }

  override def fields: List[JFieldDef] = ???

  override def methods: List[JMethodDef] = ???

  import scalaz.Scalaz._
  override def signature: JClassSignature = {
    val mps = file.state.successOrError(ast.metaParameters.traverse(resolver.metaParameter), "invalid meta parameters in class " + name, Nil)
    val sup = ast.superClass.map { s =>
      file.state.successOrError(resolver.classTypeSignature(s), "invalid super type of class " + name + " : " + s, JTypeSignature.objectTypeSig)
    }.getOrElse(JTypeSignature.objectTypeSig)
    val ifs = file.state.successOrError(ast.interfaces.traverse(resolver.classTypeSignature), "invalid interface types in class " + name, Nil)
    JClassSignature(mps, sup, ifs)
  }

  override private[ir] def implicitMethodModifier = 0
}

trait IRMethod extends JMethodDef {
  private[ir] def paramInitializers: List[IRParameterInitializer]
  def declaringClass: IRClass
  def state = declaringClass.state
}

case class IRMethodDef (ast: MethodDeclaration, declaringClass: IRClass) extends IRMethod {
  lazy val mod: JModifier = IRModifiers.mod(ast.modifiers) | declaringClass.implicitMethodModifier

  def name: String = ast.name

  override def syntax: Option[JOperatorSyntax] = ???

  override def signature: JMethodSignature = ???

  override private[ir] def paramInitializers: List[IRParameterInitializer] = ???
}

class IRFormalParameter (ast: FormalParameter, method: IRMethod) {
  //lazy val initializer: Option[IRParameterInitializer] = ast.initializer.map(snippet => IRParameterInitializer(method, ))
}

case class IRParameterInitializer (method: IRMethod, returnType: JTypeSignature, snippet: String) extends IRMethod {
  def mod = JModifier(accPublic | accStatic | accFinal)

  def syntax: Option[JOperatorSyntax] = None

  def declaringClass: IRClass = method.declaringClass

  def name: String = method.name + "$init$" + uid

  def signature: JMethodSignature = JMethodSignature(Nil, Nil, returnType, Nil, Nil, Nil, Nil)

  private[ir] def paramInitializers: List[IRParameterInitializer] = Nil

  private lazy val uid = method.state.uniqueId
}

object IRModifiers {
  def mod (modifiers: List[Modifier]): JModifier = JModifier(modifiers.foldRight(0)((m, flags) => flags | flag(m)))

  private def flag (modifier: Modifier): Int = modifier match {
    case PublicModifier       => accPublic
    case PrivateModifier      => accPrivate
    case ProtectedModifier    => accProtected
    case StaticModifier       => accStatic
    case FinalModifier        => accFinal
    case SynchronizedModifier => accSynchronized
    case VolatileModifier     => accVolatile
    case TransientModifier    => accTransient
    case NativeModifier       => accNative
    case AbstractModifier     => accAbstract
    case StrictFPModifier     => accStrict
    case _ => 0
  }
}