package phenan.prj.internal

import phenan.prj._
import phenan.prj.exception.ClassFileNotFoundException
import phenan.prj.ir.IRs
import phenan.prj.signature._

import scala.util._
import scalaz.Memo._

/**
  * Created by ichikawa on 2017/06/16.
  */
trait ClassFileLoader {
  this: JCompiler with JClassLoader with ClassFileAnalyzer with ClassFileParser with SignatureParser with DescriptorParser with IRs with JErasedTypes with Application =>

  val loadClass: String => Try[JClass] = mutableHashMapMemo(getClass)

  private def getClass(name: String): Try[JClass] = {
    findIR(name).map(Success(_)) orElse
      findAndLoadClassFile(name) orElse
      findAndCompileSourceFile(name) getOrElse
      Failure(ClassFileNotFoundException("not found : " + name))
  }

  private def findAndLoadClassFile (name: String): Option[Try[JClass]] = {
    findClassFile(name).map { resource =>
      if (config.displayLoadedClassFiles) info(s"load $name")
      parseClassFile(resource.openInputStream).map(JLoadedClass)
    }
  }

  private def findAndCompileSourceFile (name: String): Option[Try[JClass]] = findSourceFile(name).map { resource =>
    compileFile(resource.openFileReader, resource.name)
    findIR(name) match {
      case Some(ir) => Success(ir)
      case None     => Failure(ClassFileNotFoundException("not found : " + name))
    }
  }

  private case class JLoadedClass (classFile: BClassFile) extends JClass with AnnotationReader with AttributeParsers with ConstantPoolReader {
    lazy val mod = JModifier(outerInfo.map(_.mod).getOrElse(0) | classFile.mod)

    lazy val name: String = internalName.replace('/', '.').replace('$', '.')

    lazy val fields: List[JFieldDef] = classFile.fields.flatMap { field =>
      parseFieldDescriptor(readUTF(field.desc)).map(new JLoadedFieldDef(field, this, _))
    }

    lazy val methods: List[JMethodDef] = classFile.methods.flatMap { method =>
      parseMethodDescriptor(readUTF(method.desc)).map(new JLoadedMethodDef(method, this, _))
    }

    lazy val signature: JClassSignature = annotations.signature.orElse {
      attributes.signature.flatMap(sig => parseClassSignature(readUTF(sig.signature)))
    } getOrElse {
      JClassSignature(readClassNameOption(classFile.superClass), classFile.interfaces.flatMap(readClassNameOption))
    }

    lazy val internalName: String = readClassName(classFile.thisClass)

    lazy val innerClasses: Map[String, String] = innerInfo.flatMap(info => readClassNameOption(info.innerClassInfo).map(readUTF(info.innerName) -> _)).toMap

    lazy val outerClass: Option[String] = outerInfo.flatMap(info => readClassNameOption(info.outerClassInfo))

    lazy val declaredPriorities: Set[JPriority] = annotations.dsl.map(_.priorities.map(JPriority(SimpleClassTypeSignature(internalName, Nil), _))).getOrElse(Nil).toSet

    def memberPriorities: Set[JPriority] = Set.empty

    def priorityConstraints: List[List[JPriority]] = annotations.dsl.map(_.constraints).getOrElse(Nil)

    lazy val withDSLs: List[JClass] = annotations.dsl.map(_.withDSLs.flatMap(t => erase_NoFail(t))).getOrElse(Nil)

    lazy val annotations: JClassAnnotations = classAnnotations(attributes.annotations)

    private lazy val attributes = parseClassAttributes(classFile.attributes)

    private lazy val outerInfo =
      attributes.innerClasses.flatMap(attr => attr.classes.find(_.innerClassInfo == classFile.thisClass))

    private def innerInfo =
      attributes.innerClasses.toList.flatMap(_.classes.filter(info => info.outerClassInfo == classFile.thisClass && info.innerName != 0))

    override def pool: BConstantPool = classFile.pool
  }

  private class JLoadedFieldDef (val field: BField, val declaringClass: JLoadedClass, val descriptor: JTypeSignature) extends JFieldDef with AnnotationReader with AttributeParsers with ConstantPoolReader {
    lazy val mod = JModifier(field.mod)

    lazy val name: String = readUTF(field.name)

    lazy val signature: JTypeSignature = annotations.signature.orElse {
      attributes.signature.flatMap(sig => parseFieldSignature(readUTF(sig.signature)))
    } getOrElse descriptor

    lazy val annotations: JFieldAnnotations = fieldAnnotations(attributes.annotations)

    private lazy val attributes = parseFieldAttributes(field.attributes)

    override def pool: BConstantPool = declaringClass.classFile.pool
  }

  private class JLoadedMethodDef (val method: BMethod, val declaringClass: JLoadedClass, val descriptor: JMethodSignature) extends JMethodDef with AnnotationReader with AttributeParsers with ConstantPoolReader {
    lazy val mod = JModifier(method.mod)

    lazy val name: String = readUTF(method.name)

    lazy val signature: JMethodSignature = annotations.signature.orElse {
      attributes.signature.flatMap(sig => parseMethodSignature(readUTF(sig.signature)))
    } getOrElse {
      descriptor.throws(attributes.exceptions.toList.flatMap(_.exceptions.map(readClassName)))
    }

    def syntax: Option[JSyntaxDef] = annotations.operator

    lazy val annotations: JMethodAnnotations = methodAnnotations(attributes.annotations)

    private lazy val attributes = parseMethodAttributes(method.attributes)

    override def pool: BConstantPool = declaringClass.classFile.pool
  }

}
