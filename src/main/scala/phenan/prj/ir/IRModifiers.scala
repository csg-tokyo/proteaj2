package phenan.prj.ir

import phenan.prj._
import phenan.prj.decl._

import phenan.prj.JModifier._

case class IRModifiers (flags: JModifier) {
  def isStatic: Boolean = flags.check(accStatic)
}

object IRModifiers {
  def apply (modifiers: List[Modifier]): IRModifiers = {
    IRModifiers(JModifier(modifiers.foldRight(0)((mod, flags) => flags | getModifierFlag(mod))))
  }

  private def getModifierFlag (modifier: Modifier): Int = modifier match {
    case PublicModifier       => accPublic
    case PrivateModifier      => accPrivate
    case ProtectedModifier    => accProtected
    case StaticModifier       => accStatic
    case FinalModifier        => accFinal
    case SynchronizedModifier => accSynchronized
    case VolatileModifier     => accVolatile
    case TransientModifier    => accTransient
    case NativeModifier       => accNative
    case AbstractModifier     => accAbstract
    case StrictFPModifier     => accStrict
    case _ => 0
  }
}
