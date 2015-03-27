package phenan.prj.internal

import phenan.prj._
import phenan.prj.state.JState

class JLoadedClass (val classFile: BClassFile, val loader: JClassLoader)(implicit state: JState) extends JClass {

  import classFile.poolReader._
  import classFile.attrParser._
  import classFile.annReader._
  import SignatureParsers._

  lazy val mod = JModifier(outerInfo.map(_.mod).getOrElse(0) | classFile.mod)

  lazy val name = readClassName(classFile.thisClass).replace('/', '.').replace('$', '.')

  lazy val superClass = loadClassOption(classFile.superClass)

  lazy val interfaces = classFile.interfaces.flatMap(loadClassOption)

  lazy val innerClasses = innerInfo.flatMap(info => loadClassOption(info.innerClassInfo).map(readUTF(info.innerName) -> _)).toMap

  lazy val outerClass = outerInfo.flatMap(info => loadClassOption(info.outerClassInfo))

  lazy val fields = classFile.fields.flatMap { field =>
    loader.fieldDescriptor(readUTF(field.desc)).map(new JLoadedFieldDef(field, this, _))
  }

  lazy val methods = classFile.methods.flatMap { method =>
    loader.methodDescriptor(readUTF(method.desc)).map(new JLoadedMethodDef(method, this, _))
  }

  def classModule: JClassModule = JTypePool.get.getClassModule(this)

  def objectType (typeArgs: List[MetaValue]): Option[JObjectType] = JTypePool.get.getObjectType(this, typeArgs)

  def isContext = annotations.isContext

  lazy val signature =
    annotations.signature.orElse(attributes.signature.flatMap(sig => parseClassSignature(readUTF(sig.signature))))

  lazy val annotations = readClassAnnotations(attributes.annotations)

  private lazy val attributes = parseClassAttributes(classFile.attributes)

  private lazy val outerInfo =
    attributes.innerClasses.flatMap(attr => attr.classes.find(_.innerClassInfo == classFile.thisClass))

  private def innerInfo =
    attributes.innerClasses.toList.flatMap(_.classes.filter(info => info.outerClassInfo == classFile.thisClass && info.innerName != 0))

  private def loadClassOption (ref: Int): Option[JClass] = readClassNameOption(ref).flatMap(cls => loader.loadClassOption(cls))
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

  lazy val exceptions: List[JClass] = exceptionNames.flatMap(name => declaringClass.loader.loadClassOption(name))

  lazy val signature =
    annotations.signature.orElse(attributes.signature.flatMap(sig => parseMethodSignature(readUTF(sig.signature))))

  lazy val annotations = readMethodAnnotations(attributes.annotations)

  private def exceptionNames = attributes.exceptions.toList.flatMap(attr => attr.exceptions.map(readClassName))

  private lazy val attributes = parseMethodAttribute(method.attributes)
}