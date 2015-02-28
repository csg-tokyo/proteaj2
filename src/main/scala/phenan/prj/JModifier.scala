package phenan.prj

case class JModifier (flags: Int) extends AnyVal {
  override def toString: String = toFlagList(1, Nil).map(JModifier.toString).mkString(" ")

  def | (flag: Int): JModifier = JModifier(flags | flag)

  private def toFlagList(checked: Int, list: List[Int]): List[Int] = {
    if (flags < checked) list.reverse
    else {
      val flag = checked << 1
      if (check(flag)) toFlagList(flag, flag :: list)
      else toFlagList(flag, list)
    }
  }

  def check (flag: Int): Boolean = (flags & flag) == flag
}

object JModifier {
  val accPublic       = 0x0001
  val accPrivate      = 0x0002
  val accProtected    = 0x0004
  val accStatic       = 0x0008
  val accFinal        = 0x0010
  val accSuper        = 0x0020
  val accSynchronized = 0x0020
  val accVolatile     = 0x0040
  val accBridge       = 0x0040
  val accTransient    = 0x0080
  val accVarArgs      = 0x0080
  val accNative       = 0x0100
  val accInterface    = 0x0200
  val accAbstract     = 0x0400
  val accStrict       = 0x0800
  val accSynthetic    = 0x1000
  val accAnnotation   = 0x2000
  val accEnum         = 0x4000

  def toString(acc: Int): String = acc match {
    case 0x0001 => "public"
    case 0x0002 => "private"
    case 0x0004 => "protected"
    case 0x0008 => "static"
    case 0x0010 => "final"
    case 0x0020 => "synchronized"
    case 0x0040 => "volatile"
    case 0x0080 => "transient"
    case 0x0100 => "native"
    case 0x0200 => "interface"
    case 0x0400 => "abstract"
    case 0x0800 => "strictfp"
    case 0x1000 => "synthetic"
    case 0x2000 => "annotation"
    case 0x4000 => "enum"
    case _ => "unknown-modifier"
  }
}
