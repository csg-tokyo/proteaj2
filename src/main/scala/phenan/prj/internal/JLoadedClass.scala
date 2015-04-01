package phenan.prj.internal

import phenan.prj._
import phenan.prj.state.JState

class JLoadedClass (val classFile: BClassFile, val compiler: JCompiler)(implicit state: JState) extends JClass {

  import classFile.poolReader._
  import classFile.attrParser._
  import classFile.annReader._
  import SignatureParsers._

  lazy val mod = JModifier(outerInfo.map(_.mod).getOrElse(0) | classFile.mod)

  lazy val name = readClassName(classFile.thisClass).replace('/', '.').replace('$', '.')

  lazy val superClass = loadClass_PE(classFile.superClass)

  lazy val interfaces = classFile.interfaces.flatMap(loadClass_PE)

  lazy val innerClasses = innerInfo.flatMap(info => loadClass_PE(info.innerClassInfo).map(readUTF(info.innerName) -> _)).toMap

  lazy val outerClass = outerInfo.flatMap(info => loadClass_PE(info.outerClassInfo))

  lazy val fields = classFile.fields.flatMap { field =>
    compiler.classLoader.fieldDescriptor(readUTF(field.desc)).map(new JLoadedFieldDef(field, this, _))
  }

  lazy val methods = classFile.methods.flatMap { method =>
    compiler.classLoader.methodDescriptor(readUTF(method.desc)).map(new JLoadedMethodDef(method, this, _))
  }

  def objectType (typeArgs: List[MetaValue]): Option[JObjectType] = compiler.typeLoader.getObjectType(this, typeArgs)

  def isContext = annotations.isContext

  lazy val metaParameterNames: List[String] = signature.map(_.metaParams.map(_.name)).getOrElse(Nil)

  lazy val signature =
    annotations.signature.orElse(attributes.signature.flatMap(sig => parseClassSignature(readUTF(sig.signature))))

  lazy val annotations = readClassAnnotations(attributes.annotations)

  private lazy val attributes = parseClassAttributes(classFile.attributes)

  private lazy val outerInfo =
    attributes.innerClasses.flatMap(attr => attr.classes.find(_.innerClassInfo == classFile.thisClass))

  private def innerInfo =
    attributes.innerClasses.toList.flatMap(_.classes.filter(info => info.outerClassInfo == classFile.thisClass && info.innerName != 0))

  private def loadClass_PE (ref: Int): Option[JClass] = readClassNameOption(ref).flatMap(cls => compiler.classLoader.loadClass_PE(cls))
}

class JLoadedFieldDef (val field: BField, val declaringClass: JLoadedClass, val fieldType: JErasedType)(implicit state: JState) extends JFieldDef {

  import declaringClass.classFile.poolReader._
  import declaringClass.classFile.attrParser._
  import declaringClass.classFile.annReader._
  import SignatureParsers._

  lazy val mod = JModifier(field.mod)

  lazy val name = readUTF(field.name)

  lazy val signature = attributes.signature.flatMap(sig => parseFieldSignature(readUTF(sig.signature)))

  lazy val annotations = readFieldAnnotations(attributes.annotations)

  private lazy val attributes = parseFieldAttribute(field.attributes)
}

class JLoadedMethodDef (val method: BMethod, val declaringClass: JLoadedClass, val descriptor: (List[JErasedType], JErasedType))(implicit state: JState) extends JMethodDef {

  import declaringClass.classFile.poolReader._
  import declaringClass.classFile.attrParser._
  import declaringClass.classFile.annReader._
  import SignatureParsers._

  lazy val mod = JModifier(method.mod)

  lazy val name = readUTF(method.name)

  override def paramTypes = descriptor._1

  override def returnType = descriptor._2

  lazy val exceptions: List[JClass] = exceptionNames.flatMap(name => declaringClass.compiler.classLoader.loadClass_PE(name))

  lazy val signature =
    annotations.signature.orElse(attributes.signature.flatMap(sig => parseMethodSignature(readUTF(sig.signature))))

  lazy val annotations = readMethodAnnotations(attributes.annotations)

  private def exceptionNames = attributes.exceptions.toList.flatMap(attr => attr.exceptions.map(readClassName))

  private lazy val attributes = parseMethodAttribute(method.attributes)
}