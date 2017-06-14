package phenan.prj.internal

import phenan.prj.exception._
import phenan.prj.state.JState

import scala.reflect.ClassTag

class BConstantPoolReader (pool: BConstantPool)(implicit state: JState) {

  def readAs[T <: BConstant] (index: Int)(implicit tag: ClassTag[T]): T = tag.unapply(pool.constants(index)).getOrElse {
    state.error("constant pool is broken")
    throw InvalidClassFileException("constant pool is broken")
  }

  def readAsOption[T <: BConstant] (index: Int)(implicit tag: ClassTag[T]): Option[T] = {
    if (index == 0) None
    else tag.unapply(pool.constants(index))
  }

  def readUTF (ref: Int): String = readAs[BUtf8Value](ref).value

  def readClassName (ref: Int): String = readAs[BUtf8Value](readAs[BClassRef](ref).ref).value
  def readClassNameOption (ref: Int): Option[String] = readAsOption[BClassRef](ref).flatMap(r => readAsOption[BUtf8Value](r.ref).map(_.value))
}
