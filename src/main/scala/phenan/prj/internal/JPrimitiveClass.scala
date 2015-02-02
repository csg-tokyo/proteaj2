package phenan.prj.internal

import phenan.prj._
import JModifier._
import phenan.prj.exception.InvalidTypeException

import scala.util.{Failure, Success, Try}

class JPrimitiveClass (val name: String, val wrapperName: String, val loader: JClassLoader) extends JClass {
  override def mod = JModifier(accPublic | accFinal)

  override def superClass: Option[JClass] = None
  override def interfaces: List[JClass] = Nil

  override def innerClasses: Map[String, JClass] = Map.empty
  override def outerClass: Option[JClass] = None

  override def fields: List[JFieldDef] = Nil
  override def methods: List[JMethodDef] = Nil

  private[internal] lazy val wrapperClass = loader.loadClass(wrapperName)

  override def classType: JClassType = ???

  override def objectType(typeArgs: List[JValueType]): Try[JValueType] = {
    if (typeArgs.isEmpty) Success(primitiveType)
    else Failure(InvalidTypeException("primitive type " + name + " does not take type arguments"))
  }

  lazy val primitiveType: JPrimitiveType = ???
}
