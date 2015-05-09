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

  def simpleName = ast.name

  private[ir] def implicitMethodModifier: Int
  private[ir] def metaParameters = ast.metaParameters

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

  lazy val formalMetaParameters = state.successOrError(metaParameters.traverse(resolver.metaParameter), "invalid meta parameters of class " + name, Nil)

  lazy val superTypeSignature = ast.superClass.map { s =>
    state.successOrError(resolver.classTypeSignature(s), "invalid super type of class " + name + " : " + s, JTypeSignature.objectTypeSig)
  }.getOrElse(JTypeSignature.objectTypeSig)

  lazy val interfaceSignatures = state.successOrError(ast.interfaces.traverse(resolver.classTypeSignature), "invalid interface types of class " + name, Nil)

  lazy val signature: JClassSignature = JClassSignature(formalMetaParameters, superTypeSignature, interfaceSignatures)

  override private[ir] def implicitMethodModifier = 0
}

trait IRMethod extends JMethodDef {
  def declaringClass: IRClass
  def state = declaringClass.state

  private[ir] def metaParameters: List[MetaParameter]
  private[ir] def paramInitializers: List[IRParameterInitializer]

  lazy val resolver = declaringClass.resolver.inMethod(this)
}

case class IRMethodDef (ast: MethodDeclaration, declaringClass: IRClass) extends IRMethod {
  lazy val mod: JModifier = IRModifiers.mod(ast.modifiers) | declaringClass.implicitMethodModifier

  def name: String = ast.name

  override def syntax: Option[JOperatorSyntax] = ???

  lazy val signature: JMethodSignature =
    JMethodSignature(formalMetaParameters, formalParameterSignatures, returnTypeSignature,
      throwsTypeSignatures, activatesTypeSignatures, deactivatesTypeSignatures, requiresTypeSignatures)

  import scalaz.Scalaz._

  lazy val formalMetaParameters = state.successOrError(metaParameters.traverse(resolver.metaParameter), "invalid meta parameters of method " + name, Nil)

  lazy val formalParameters = ast.formalParameters.map(param => new IRFormalParameter(param, this))

  lazy val formalParameterSignatures = state.successOrError(formalParameters.traverse(_.signature), "invalid parameter types of method " + name, Nil)

  lazy val returnTypeSignature = state.successOrError(resolver.typeSignature(ast.returnType), "invalid return type of method " + name, VoidTypeSignature)

  lazy val throwsTypeSignatures = ast.clauses.collectFirst { case ThrowsClause(es) =>
    state.successOrError(es.traverse(resolver.typeSignature), "invalid throws clause of method " + name, Nil)
  }.getOrElse(Nil)

  lazy val activatesTypeSignatures = ast.clauses.collectFirst { case ActivatesClause(cs) =>
    state.successOrError(cs.traverse(resolver.typeSignature), "invalid activates clause of method " + name, Nil)
  }.getOrElse(Nil)

  lazy val deactivatesTypeSignatures = ast.clauses.collectFirst { case DeactivatesClause(cs) =>
    state.successOrError(cs.traverse(resolver.typeSignature), "invalid deactivates clause of method " + name, Nil)
  }.getOrElse(Nil)

  lazy val requiresTypeSignatures = ast.clauses.collectFirst { case RequiresClause(cs) =>
    state.successOrError(cs.traverse(resolver.typeSignature), "invalid requires clause of method " + name, Nil)
  }.getOrElse(Nil)

  private[ir] def metaParameters: List[MetaParameter] = ast.metaParameters

  private[ir] def paramInitializers: List[IRParameterInitializer] = formalParameters.flatMap(_.initializerMethod)
}

class IRFormalParameter (ast: FormalParameter, method: IRMethod) {
  private lazy val initializer = ast.initializer.map(snippet => (method.name + "$init$" + method.state.uniqueId, snippet))
  lazy val signature = method.resolver.parameterSignature(ast, initializer.map(_._1))
  lazy val initializerMethod = for {
    returnType <- signature.map(_.actualTypeSignature).toOption
    (name, snippet) <- initializer
  } yield IRParameterInitializer(method, returnType, name, snippet.snippet)
}

case class IRParameterInitializer (method: IRMethod, returnType: JTypeSignature, name: String, snippet: String) extends IRMethod {
  def mod = JModifier(accPublic | accStatic | accFinal)

  def syntax: Option[JOperatorSyntax] = None

  def declaringClass: IRClass = method.declaringClass

  def signature: JMethodSignature = JMethodSignature(Nil, Nil, returnType, Nil, Nil, Nil, Nil)

  private[ir] def metaParameters: List[MetaParameter] = Nil
  private[ir] def paramInitializers: List[IRParameterInitializer] = Nil
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