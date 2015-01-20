package phenan.jir.internal

import phenan.jir._

class JLoadedClass (val classFile: BClassFile, val loader: JClassLoader) extends JClass {

  import classFile.poolReader._
  import classFile.attrParser._
  import SignatureParsers._

  lazy val mod = JModifier(outerInfo.map(_.mod).getOrElse(0) | classFile.mod)

  lazy val name = readClassName(classFile.thisClass).replace('/', '.').replace('$', '.')

  lazy val superClass = loadClassOption(classFile.superClass)

  lazy val interfaces = classFile.interfaces.map(loadClass)

  lazy val innerClasses = innerInfo.map(info => readUTF(info.innerName) -> loadClass(info.innerClassInfo)).toMap

  lazy val outerClass = outerInfo.map(info => loadClass(info.outerClassInfo))

  lazy val fields = classFile.fields.map(field => new JLoadedFieldDef(field, this))

  lazy val methods = classFile.methods.map(method => new JLoadedMethodDef(method, this))

  lazy val signature = attributes.signature.map(sig => parseClassSignature(readUTF(sig.signature)).get)

  private lazy val attributes = parseClassAttributes(classFile.attributes)

  private lazy val outerInfo =
    attributes.innerClasses.flatMap(attr => attr.classes.find(_.innerClassInfo == classFile.thisClass))

  private def innerInfo =
    attributes.innerClasses.toList.flatMap(_.classes.filter(info => info.outerClassInfo == classFile.thisClass && info.innerName != 0))

  private def loadClassOption (ref: Int): Option[JClass] = readClassNameOption(ref).map(cls => loader.load(cls).get)
  private def loadClass (ref: Int): JClass = loader.load(readClassName(ref)).get
}

class JLoadedFieldDef (val field: BField, val declaringClass: JLoadedClass) extends JFieldDef {

  import declaringClass.classFile.poolReader._
  import declaringClass.classFile.attrParser._
  import SignatureParsers._

  lazy val mod = JModifier(field.mod)

  lazy val name = readUTF(field.name)

  lazy val fieldClass = declaringClass.loader.fieldDescriptor(readUTF(field.desc)).get

  lazy val signature = attributes.signature.map(sig => parseFieldSignature(readUTF(sig.signature)).get)

  private lazy val attributes = parseFieldAttribute(field.attributes)
}

class JLoadedMethodDef (val method: BMethod, val declaringClass: JLoadedClass) extends JMethodDef {

  import declaringClass.classFile.poolReader._
  import declaringClass.classFile.attrParser._
  import SignatureParsers._

  lazy val mod = JModifier(method.mod)

  lazy val name = readUTF(method.name)

  override def paramClasses = descriptor._1

  override def returnClass  = descriptor._2

  lazy val exceptions = exceptionNames.map(name => declaringClass.loader.loadClass(name).get)

  lazy val signature = attributes.signature.map(sig => parseMethodSignature(readUTF(sig.signature)))

  private def exceptionNames = attributes.exceptions.toList.flatMap(attr => attr.exceptions.map(readClassName))

  private lazy val attributes = parseMethodAttribute(method.attributes)
  private lazy val descriptor = declaringClass.loader.methodDescriptor(readUTF(method.desc)).get
}