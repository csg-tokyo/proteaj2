package phenan.prj.ir

import phenan.prj._
import phenan.prj.decl._
import phenan.prj.exception.InvalidTypeException
import phenan.prj.internal.JClassLoader
import phenan.prj.state.JState
import phenan.util._

import scala.util._
import scalaz.Scalaz._
import scalaz.Memo._

class IRResolver (header: Header, val loader: JClassLoader)(implicit state: JState) {
  def packageName: Option[String] = header.pack.map(pack => pack.name.names.mkString("."))

  val packageInternalName: Option[String] = header.pack.map(pack => pack.name.names.mkString("/"))

  lazy val importedClasses: Map[String, JClass] = {
    header.imports.flatMap {
      case SingleClassImportDeclaration(name) => tryLoadClass(name.names) match {
        case Success(c) => Some(name.names.last -> c)
        case Failure(e) =>
          state.error("class not found : " + name.names.mkString("."))
          None
      }
      case _ => None
    }.toMap
  }

  val loadClass: List[String] => Try[JClass] = mutableHashMapMemo(loadClassWithoutCache)

  def loadClassWithoutCache (name: List[String]): Try[JClass] = {
    if (importedClasses.contains(name.head)) tryGetInnerClass(importedClasses(name.head), name.tail)
    else tryLoadClassFromPackage(name, packages)
  }

  def arrayOf (t: JErasedType): JArrayClass = loader.arrayOf(t)

  def arrayOf (t: IRGenericType, dim: Int): IRGenericType = {
    if (dim > 1) arrayOf(IRGenericArrayType(t, this), dim - 1)
    else t
  }

  def voidClass = loader.void
  def objectClass = loader.objectClass

  def primitives = loader.primitives

  private[ir] def declareTypeVariables (parameters: List[TypeParameter], env: Map[String, IRTypeVariable]): Map[String, IRTypeVariable] = {
    parameters.foldLeft (env) { (env, param) =>
      env + (param.name -> IRTypeParameter(param, env, this).typeVariable)
    }
  }

  private[ir] def typeName (t: TypeName, typeVariables: Map[String, IRTypeVariable]): Try[IRGenericType] = t match {
    case c : ClassTypeName => simpleTypeName(c, typeVariables)
    case a : ArrayTypeName => arrayTypeName(a, typeVariables)
  }

  private[ir] def simpleTypeName (c: ClassTypeName, typeVariables: Map[String, IRTypeVariable]): Try[IRGenericType] = {
    if (c.args.isEmpty && c.name.names.size == 1) {
      val name = c.name.names.head
      if (primitives.contains(name)) Success(IRGenericPrimitiveType(primitives(name)))
      else if (typeVariables.contains(name)) Success(typeVariables(name))
      else classTypeName(c, typeVariables)
    }
    else classTypeName(c, typeVariables)
  }

  private[ir] def refTypeNames (cs: List[ClassTypeName], typeVariables: Map[String, IRTypeVariable]): Try[List[IRGenericRefType]] = {
    cs.traverse(refTypeName(_, typeVariables))
  }

  private[ir] def refTypeName (c: ClassTypeName, typeVariables: Map[String, IRTypeVariable]): Try[IRGenericRefType] = {
    if (c.args.isEmpty && c.name.names.size == 1 && typeVariables.contains(c.name.names.head)) Success(typeVariables(c.name.names.head))
    else classTypeName(c, typeVariables)
  }

  private[ir] def classTypeName (c: ClassTypeName, typeVariables: Map[String, IRTypeVariable]): Try[IRGenericClassType] = for {
    clazz <- loadClass(c.name.names)
    args  <- c.args.traverse(typeArgument(_, typeVariables))
  } yield IRGenericClassType(clazz, args)

  private[ir] def arrayTypeName (a: ArrayTypeName, typeVariables: Map[String, IRTypeVariable]): Try[IRGenericArrayType] = typeName(a.component, typeVariables).map(IRGenericArrayType(_, this))

  private[ir] def typeArgument (arg: TypeArgument, typeVariables: Map[String, IRTypeVariable]): Try[IRTypeArgument] = arg match {
    case t: TypeName                => typeName(t, typeVariables)
    case WildcardType(upper, lower) => for {
      u <- upper.traverse(typeName(_, typeVariables))
      l <- lower.traverse(typeName(_, typeVariables))
    } yield IRGenericWildcardType(u, l)
  }

  private def tryLoadClass (name: List[String]): Try[JClass] = tryLoadClass(name.head, name.tail)

  private def tryLoadClass (name: String, rest: List[String]): Try[JClass] = loader.loadClass(name) match {
    case Success(c) if rest.nonEmpty => tryGetInnerClass(c, rest)
    case Failure(e) if rest.nonEmpty => tryLoadClass(name + '/' + rest.head, rest.tail)
    case Success(c) => Success(c)
    case Failure(e) => Failure(e)
  }

  private def tryGetInnerClass (clazz: JClass, name: List[String]): Try[JClass] = {
    if (name.isEmpty) Success(clazz)
    else if (clazz.innerClasses.contains(name.head)) tryGetInnerClass(clazz.innerClasses(name.head), name.tail)
    else Failure(InvalidTypeException("inner class " + name.head + " is not found"))
  }

  private def tryLoadClassFromPackage (name: List[String], packages: List[QualifiedName]): Try[JClass] = packages match {
    case pack :: rest => tryLoadClass(pack.names ++ name) match {
      case Success(clazz) => Success(clazz)
      case Failure(e) => tryLoadClassFromPackage(name, rest)
    }
    case Nil => tryLoadClass(name)
  }

  private val packages = header.pack match {
    case Some(pack) => pack.name :: header.imports.collect {
      case PackageImportDeclaration(name) => name
    }
    case None => header.imports.collect {
      case PackageImportDeclaration(name) => name
    }
  }
}
