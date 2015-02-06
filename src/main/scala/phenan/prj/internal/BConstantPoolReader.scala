package phenan.prj.internal

import com.typesafe.scalalogging._

import phenan.prj.exception._

import scala.language.implicitConversions
import scala.reflect.ClassTag

class BConstantPoolReader (pool: BConstantPool) extends LazyLogging {

  def readAs[T <: BConstant] (index: Int)(implicit tag: ClassTag[T]): T = tag.unapply(pool.constants(index)).getOrElse {
    logger.error("constant pool is broken")
    throw InvalidClassFileException("constant pool is broken")
  }

  def readAsOption[T <: BConstant] (index: Int)(implicit tag: ClassTag[T]): Option[T] = {
    if (index == 0) None
    else tag.unapply(pool.constants(index))
  }

  def readUTF (ref: Int): String = readAs[BUtf8Value](ref).value

  def readClassName (ref: Int): String = readAs[BUtf8Value](readAs[BClassRef](ref).ref).value
  def readClassNameOption (ref: Int): Option[String] = readAsOption[BClassRef](ref).map(r => readUTF(r.ref))
}
