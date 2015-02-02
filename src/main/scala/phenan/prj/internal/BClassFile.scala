package phenan.prj.internal

case class BClassFile (minor: Int, major: Int, pool: BConstantPool, mod: Int,
  thisClass: Int, superClass: Int, interfaces: List[Int],
  fields: List[BField], methods: List[BMethod], attributes: List[BAttributeInfo] )
{
  lazy val poolReader = new BConstantPoolReader(pool)
  lazy val attrParser = new BAttributeParsers(this)
}

case class BField (mod: Int, name: Int, desc: Int, attributes: List[BAttributeInfo])

case class BMethod (mod: Int, name: Int, desc: Int, attributes: List[BAttributeInfo])

case class BAttributeInfo (tag: Int, data: Array[Byte])

case class BConstantPool (constants: Map[Int, BConstant])

sealed trait BConstant

case class BIntValue (value: Int) extends BConstant
case class BFloatValue (value: Float) extends BConstant
case class BLongValue (value: Long) extends BConstant
case class BDoubleValue (value: Double) extends BConstant
case class BUtf8Value (value: String) extends BConstant

case class BStringRef (ref: Int) extends BConstant
case class BClassRef (ref: Int) extends BConstant
case class BNameTypeRef (nameRef: Int, typeRef: Int) extends BConstant

case class BFieldRef (classRef: Int, sigRef: Int) extends BConstant
case class BMethodRef (classRef: Int, sigRef: Int) extends BConstant
case class BIMethodRef (classRef: Int, sigRef: Int) extends BConstant
