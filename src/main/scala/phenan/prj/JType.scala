package phenan.prj

sealed trait JType {
  def fields: Map[String, JField]
  def methods: Map[String, List[JMethod]]
}

trait JClassType extends JType

sealed trait JValueType extends JType {
  def name: String
  def array: JArrayType

  def isSubtypeOf (that: JValueType): Boolean
  def isAssignableTo (that: JValueType): Boolean

  def <:< (t: JValueType): Boolean = this.isSubtypeOf(t)
  def >:> (t: JValueType): Boolean = t.isSubtypeOf(this)
}

trait JObjectType extends JValueType {
  def erase: JClass

  def typeArguments: Map[String, JValueType]

  def superType: Option[JObjectType]
  def interfaceTypes: List[JObjectType]

  def declaredFields: List[JField]
  def declaredMethods: List[JMethod]

  def constructors: List[JConstructor]

  lazy val privateFields: Map[String, JField] = declaredFields.filter(_.isPrivate).map(f => f.name -> f).toMap
  lazy val privateMethods: Map[String, List[JMethod]] = declaredMethods.filter(_.isPrivate).groupBy(_.name)

  lazy val fields: Map[String, JField] = nonPrivateFieldList.map(f => f.name -> f).toMap
  lazy val methods: Map[String, List[JMethod]] = nonPrivateMethodList.groupBy(_.name).mapValues(filterOutOverriddenMethod)

  def isSubtypeOf (that: JValueType): Boolean = that match {
    case _ if this == that   => true
    case that: JObjectType   => isSubtypeOf(that)
    case that: JWildcardType => that.lowerBound.exists(lb => isSubtypeOf(lb))
    case _                   => false
  }

  def isSubtypeOf (that: JObjectType): Boolean = {
    isMatchedTo(that) || superType.exists(_.isSubtypeOf(that)) || interfaceTypes.exists(_.isSubtypeOf(that))
  }

  def isMatchedTo (that: JObjectType): Boolean = {
    if (this.erase == that.erase) that match {
      case that: JObjectType => matchTypeArgs(that.typeArguments)
      case _                 => false
    }
    else false
  }

  /* helper methods for collecting non-private inherited members */

  private def nonPrivateFieldList: List[JField] = {
    interfaceTypes.map(_.nonPrivateFieldList).reduceLeftOption(_ ++ _).getOrElse(Nil) ++
      superType.map(_.nonPrivateFieldList).getOrElse(Nil) ++
      declaredFields.filterNot(_.isPrivate)
  }

  private def nonPrivateMethodList: List[JMethod] = {
    interfaceTypes.map(_.nonPrivateMethodList).reduceLeftOption(_ ++ _).getOrElse(Nil) ++
      superType.map(_.nonPrivateMethodList).getOrElse(Nil) ++
      declaredMethods.filterNot(_.isPrivate)
  }

  private def filterOutOverriddenMethod (list: List[JMethod]): List[JMethod] = {
    list.foldRight[List[JMethod]](Nil) { (m, ms) =>
      if (ms.exists(_.overrides(m))) ms
      else m :: ms
    }
  }

  private def matchTypeArgs (args: Map[String, JValueType]): Boolean = typeArguments.forall { case (key, value) =>
    args.get(key) match {
      case Some(arg) if value == arg => true
      case Some(arg: JWildcardType)  => arg.includes(value)
      case _                         => false
    }
  }
}

trait JPrimitiveType extends JValueType {
  def wrapperType: JValueType

  def fields: Map[String, JField] = Map.empty
  def methods: Map[String, List[JMethod]] = Map.empty

  def isSubtypeOf (that: JValueType): Boolean = this == that
}

trait JArrayType extends JValueType {
  def componentType: JValueType
  def superType: JObjectType
  def interfaceTypes: List[JObjectType]

  def isSubtypeOf (that: JValueType): Boolean = that match {
    case _ if this == that   => true
    case that: JArrayType    => componentType.isSubtypeOf(that.componentType)
    case that: JObjectType   => superType.isSubtypeOf(that) || interfaceTypes.exists(_.isSubtypeOf(that))
    case that: JWildcardType => that.lowerBound.exists(lb => isSubtypeOf(lb))
    case _                   => false
  }

  def isAssignableTo (that: JValueType): Boolean = isSubtypeOf(that)
}

trait JWildcardType extends JValueType {
  def upperBound: JValueType
  def lowerBound: Option[JValueType]

  def isSubtypeOf (that: JValueType): Boolean = upperBound.isSubtypeOf(that)
  def isAssignableTo (that: JValueType): Boolean = isSubtypeOf(that)

  def includes (that: JValueType): Boolean = that.isSubtypeOf(upperBound) && lowerBound.forall(_.isSubtypeOf(that))
}
