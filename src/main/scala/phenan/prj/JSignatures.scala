package phenan.prj

case class JClassSignature (metaParams: List[FormalMetaParameter], superClass: JClassTypeSignature, interfaces: List[JClassTypeSignature]) {
  override def toString: String = {
    metaParams.mkString("<", ", ", ">") + " : " + superClass + interfaces.map(" : " + _).mkString
  }
}

object JClassSignature {
  def apply (superClassName: Option[String], interfaceNames: List[String]): JClassSignature = {
    JClassSignature(Nil, superClassName.map(SimpleClassTypeSignature(_, Nil)).getOrElse(JTypeSignature.objectTypeSig), interfaceNames.map(SimpleClassTypeSignature(_, Nil)))
  }
}

case class JMethodSignature (metaParams: List[FormalMetaParameter], parameters: List[JParameterSignature], returnType: JTypeSignature,
                             returnBounds: List[JTypeSignature], throwTypes: List[JClassTypeSignature],
                             activates: List[JClassTypeSignature], deactivates: List[JClassTypeSignature], requires: List[JClassTypeSignature]) {
  def throws (es: List[String]): JMethodSignature = {
    if (es.nonEmpty) JMethodSignature(metaParams, parameters, returnType, returnBounds, throwTypes ++ es.map(name => SimpleClassTypeSignature(name, Nil)), activates, deactivates, requires)
    else this
  }
}

case class JParameterSignature (contexts: List[JTypeSignature], without: List[JTypeSignature], typeSig: JTypeSignature, varArgs: Boolean, defaultArg: Option[String], scopes: List[JClassTypeSignature]) {
  def actualTypeSignature: JTypeSignature = {
    val target = if (varArgs) JArrayTypeSignature(typeSig) else typeSig
    contexts.foldRight(target)(JTypeSignature.functionTypeSig)
  }
}

case class FormalMetaParameter (name: String, metaType: JTypeSignature, bounds: List[JTypeSignature]) {
  override def toString: String = {
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
  def consumerTypeSig (arg: JTypeArgument): JTypeSignature = SimpleClassTypeSignature(CommonNames.consumerClassName, List(arg))
  def enumTypeSig (e: JTypeArgument): JClassTypeSignature = SimpleClassTypeSignature(CommonNames.enumClassName, List(e))
  def pairTypeSig (car: JTypeArgument, cdr: JTypeArgument): JTypeSignature = SimpleClassTypeSignature(CommonNames.pairClassName, List(car, cdr))

  def arraySig (typeSig: JTypeSignature, dim: Int): JTypeSignature = {
    if (dim > 0) arraySig(JArrayTypeSignature(typeSig), dim - 1)
    else typeSig
  }
}

sealed trait JClassTypeSignature extends JTypeSignature {
  def internalName: String
}

case class SimpleClassTypeSignature (clazz: String, args: List[JTypeArgument]) extends JClassTypeSignature {
  def internalName: String = clazz
}

case class MemberClassTypeSignature (outer: JClassTypeSignature, clazz: String, args: List[JTypeArgument]) extends JClassTypeSignature {
  def internalName: String = outer.internalName + '$' + clazz
}

sealed trait JPrimitiveTypeSignature extends JTypeSignature

case object ByteTypeSignature extends JPrimitiveTypeSignature
case object CharTypeSignature extends JPrimitiveTypeSignature
case object DoubleTypeSignature extends JPrimitiveTypeSignature
case object FloatTypeSignature extends JPrimitiveTypeSignature
case object IntTypeSignature extends JPrimitiveTypeSignature
case object LongTypeSignature extends JPrimitiveTypeSignature
case object ShortTypeSignature extends JPrimitiveTypeSignature
case object BoolTypeSignature extends JPrimitiveTypeSignature
case object VoidTypeSignature extends JPrimitiveTypeSignature

case class JArrayTypeSignature (component: JTypeSignature) extends JTypeSignature

case class JTypeVariableSignature (name: String) extends JTypeSignature

case class JCapturedWildcardSignature (upperBound: Option[JTypeSignature], lowerBound: Option[JTypeSignature]) extends JTypeSignature

sealed trait JTypeArgument

case class MetaVariableSignature (name: String) extends JTypeArgument

case class WildcardArgument (upperBound: Option[JTypeSignature], lowerBound: Option[JTypeSignature]) extends JTypeArgument
