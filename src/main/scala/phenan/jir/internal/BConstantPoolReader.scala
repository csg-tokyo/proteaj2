package phenan.jir.internal

import phenan.jir.exception._

import scala.language.implicitConversions
import scala.reflect.ClassTag

class BConstantPoolReader (pool: BConstantPool) {

  def readAs[T <: BConstant] (index: Int)(implicit tag: ClassTag[T]): T = tag.unapply(pool.constants(index)).getOrElse {
    throw InvalidClassFileException("constant pool is broken")
  }

  def readAsOption[T <: BConstant] (index: Int)(implicit tag: ClassTag[T]): Option[T] = {
    if (index == 0) None
    else tag.unapply(pool.constants(index))
  }

  def readUTF (ref: Int): String = readAs[BUtf8Value](ref).value

  def readClassName (ref: Int): String = readAs[BUtf8Value](readAs[BClassRef](ref).ref).value
  def readClassNameOption (ref: Int): Option[String] = readAsOption[BClassRef](ref).map(r => readAs[BUtf8Value](r.ref).value)
}
