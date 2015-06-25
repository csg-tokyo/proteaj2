package phenan.prj.internal

import phenan.prj._
import phenan.prj.signature._

class JLoadedClass (val classFile: BClassFile, val compiler: JCompiler) extends JClass {

  import classFile.poolReader._
  import classFile.attrParser._
  import classFile.annReader._
  import DescriptorParsers._
  import SignatureParsers._

  lazy val mod = JModifier(outerInfo.map(_.mod).getOrElse(0) | classFile.mod)

  lazy val name = internalName.replace('/', '.').replace('$', '.')

  lazy val fields = classFile.fields.flatMap { field =>
    parseFieldDescriptor(readUTF(field.desc)).map(new JLoadedFieldDef(field, this, _))
  }

  lazy val methods = classFile.methods.flatMap { method =>
    parseMethodDescriptor(readUTF(method.desc)).map(new JLoadedMethodDef(method, this, _))
  }

  def isContext = annotations.isContext

  lazy val signature = annotations.signature.orElse {
    attributes.signature.flatMap(sig => parseClassSignature(readUTF(sig.signature)))
  } getOrElse {
    JClassSignature(readClassNameOption(classFile.superClass), classFile.interfaces.flatMap(readClassNameOption))
  }

  lazy val internalName = readClassName(classFile.thisClass)

  lazy val innerClasses = innerInfo.flatMap(info => readClassNameOption(info.innerClassInfo).map(readUTF(info.innerName) -> _)).toMap

  lazy val outerClass = outerInfo.flatMap(info => readClassNameOption(info.outerClassInfo))

  lazy val declaredPriorities = annotations.dsl.map(_.priorities.map(JPriority(SimpleClassTypeSignature(internalName, Nil), _))).getOrElse(Nil).toSet

  def memberPriorities = Set.empty

  lazy val priorityConstraints = annotations.dsl.map(_.constraints).getOrElse(Nil)

  lazy val annotations = classAnnotations(attributes.annotations)

  private lazy val attributes = parseClassAttributes(classFile.attributes)

  private lazy val outerInfo =
    attributes.innerClasses.flatMap(attr => attr.classes.find(_.innerClassInfo == classFile.thisClass))

  private def innerInfo =
    attributes.innerClasses.toList.flatMap(_.classes.filter(info => info.outerClassInfo == classFile.thisClass && info.innerName != 0))
}

class JLoadedFieldDef (val field: BField, val declaringClass: JLoadedClass, val descriptor: JTypeSignature) extends JFieldDef {

  import declaringClass.classFile.poolReader._
  import declaringClass.classFile.attrParser._
  import declaringClass.classFile.annReader._
  import SignatureParsers._

  lazy val mod = JModifier(field.mod)

  lazy val name = readUTF(field.name)

  lazy val signature = annotations.signature.orElse {
    attributes.signature.flatMap(sig => parseFieldSignature(readUTF(sig.signature)))
  } getOrElse descriptor

  lazy val annotations = fieldAnnotations(attributes.annotations)

  private lazy val attributes = parseFieldAttribute(field.attributes)
}

class JLoadedMethodDef (val method: BMethod, val declaringClass: JLoadedClass, val descriptor: JMethodSignature) extends JMethodDef {

  import declaringClass.classFile.poolReader._
  import declaringClass.classFile.attrParser._
  import declaringClass.classFile.annReader._
  import SignatureParsers._

  lazy val mod = JModifier(method.mod)

  lazy val name = readUTF(method.name)

  lazy val signature = annotations.signature.orElse {
    attributes.signature.flatMap(sig => parseMethodSignature(readUTF(sig.signature)))
  } getOrElse {
    descriptor.throws(attributes.exceptions.toList.flatMap(_.exceptions.map(readClassName)))
  }

  def syntax = annotations.operator

  lazy val annotations = methodAnnotations(attributes.annotations)

  private lazy val attributes = parseMethodAttribute(method.attributes)
}