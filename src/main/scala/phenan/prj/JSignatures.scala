package phenan.prj

case class JClassSignature (metaParams: List[FormalMetaParameter], superClass: JClassTypeSignature, interfaces: List[JClassTypeSignature]) {
  override def toString = {
    metaParams.mkString("<", ", ", ">") + " : " + superClass + interfaces.map(" : " + _).mkString
  }
}

object JClassSignature {
  def apply (superClassName: Option[String], interfaceNames: List[String]): JClassSignature = {
    JClassSignature(Nil, superClassName.map(SimpleClassTypeSignature(_, Nil)).getOrElse(JTypeSignature.objectTypeSig), interfaceNames.map(SimpleClassTypeSignature(_, Nil)))
  }
}

case class JMethodSignature (metaParams: List[FormalMetaParameter], parameters: List[JParameterSignature], returnType: JTypeSignature,
                             returnBounds: List[JTypeSignature], throwTypes: List[JTypeSignature],
                             activates: List[JTypeSignature], deactivates: List[JTypeSignature], requires: List[JTypeSignature]) {
  def throws (es: List[String]): JMethodSignature = {
    if (es.nonEmpty) JMethodSignature(metaParams, parameters, returnType, returnBounds, throwTypes ++ es.map(name => SimpleClassTypeSignature(name, Nil)), activates, deactivates, requires)
    else this
  }
}

case class JParameterSignature (contexts: List[JTypeSignature], typeSig: JTypeSignature, varArgs: Boolean, defaultArg: Option[String]) {
  def actualTypeSignature: JTypeSignature = {
    val target = if (varArgs) JArrayTypeSignature(typeSig) else typeSig
    contexts.foldRight(target)(JTypeSignature.functionTypeSig)
  }
  override def toString = {
    val cs = contexts.map('@' + _.toString).mkString
    val da = defaultArg.map('?' + _).mkString
    if(varArgs) cs + typeSig.toString + '*' + da
    else cs + typeSig.toString + da
  }
}

case class FormalMetaParameter (name: String, metaType: JTypeSignature, bounds: List[JTypeSignature]) {
  override def toString = {
    if (bounds.nonEmpty) name + bounds.mkString(" extends ", " & ", "")
    else if (metaType == JTypeSignature.typeTypeSig) name
    else name + " : " + metaType
  }
}

sealed trait JTypeSignature extends JTypeArgument

object JTypeSignature {
  lazy val typeTypeSig = SimpleClassTypeSignature(CommonNames.typeClassName, Nil)
  lazy val classSigTypeSig = SimpleClassTypeSignature(CommonNames.classSigClassName, Nil)
  lazy val objectTypeSig = SimpleClassTypeSignature(CommonNames.objectClassName, Nil)
  lazy val boxedVoidTypeSig = SimpleClassTypeSignature(CommonNames.boxedVoidClassName, Nil)

  def functionTypeSig (from: JTypeArgument, to: JTypeArgument): JTypeSignature = SimpleClassTypeSignature(CommonNames.functionClassName, List(from, to))
  def enumTypeSig (e: JTypeArgument): JClassTypeSignature = SimpleClassTypeSignature(CommonNames.enumClassName, List(e))

  def arraySig (typeSig: JTypeSignature, dim: Int): JTypeSignature = {
    if (dim > 0) arraySig(JArrayTypeSignature(typeSig), dim - 1)
    else typeSig
  }
}

sealed trait JClassTypeSignature extends JTypeSignature {
  def internalName: String
  def signatureString: String
  override def toString = 'L' + signatureString + ';'
}

case class SimpleClassTypeSignature (clazz: String, args: List[JTypeArgument]) extends JClassTypeSignature {
  def internalName = clazz
  def signatureString: String = {
    if (args.isEmpty) clazz
    else clazz + args.mkString("<", "", ">")
  }
}

case class MemberClassTypeSignature (outer: JClassTypeSignature, clazz: String, args: List[JTypeArgument]) extends JClassTypeSignature {
  def internalName = outer.internalName + '$' + clazz
  def signatureString: String = {
    if (args.isEmpty) outer.signatureString + '.' + clazz
    else outer.signatureString + '.' + clazz + args.mkString("<", "", ">")
  }
}

sealed trait JPrimitiveTypeSignature extends JTypeSignature {
  def symbol: Char
  override def toString = symbol.toString
}

case object ByteTypeSignature extends JPrimitiveTypeSignature { def symbol = 'B' }
case object CharTypeSignature extends JPrimitiveTypeSignature { def symbol = 'C' }
case object DoubleTypeSignature extends JPrimitiveTypeSignature { def symbol = 'D' }
case object FloatTypeSignature extends JPrimitiveTypeSignature { def symbol = 'F' }
case object IntTypeSignature extends JPrimitiveTypeSignature { def symbol = 'I' }
case object LongTypeSignature extends JPrimitiveTypeSignature { def symbol = 'J' }
case object ShortTypeSignature extends JPrimitiveTypeSignature { def symbol = 'S' }
case object BoolTypeSignature extends JPrimitiveTypeSignature { def symbol = 'Z' }
case object VoidTypeSignature extends JPrimitiveTypeSignature { def symbol = 'V' }

case class JArrayTypeSignature (component: JTypeSignature) extends JTypeSignature {
  override def toString = '[' + component.toString
}

case class JTypeVariableSignature (name: String) extends JTypeSignature {
  override def toString = 'T' + name + ';'
}

case class JCapturedWildcardSignature (upperBound: Option[JTypeSignature], lowerBound: Option[JTypeSignature]) extends JTypeSignature

sealed trait JTypeArgument {
  def toString: String
}

case class MetaVariableSignature (name: String) extends JTypeArgument

case class WildcardArgument (upperBound: Option[JTypeSignature], lowerBound: Option[JTypeSignature]) extends JTypeArgument {
  override def toString = upperBound match {
    case Some(ub) => '+' + ub.toString
    case None => lowerBound match {
      case Some(lb) => '-' + lb.toString
      case None => "*"
    }
  }
}
