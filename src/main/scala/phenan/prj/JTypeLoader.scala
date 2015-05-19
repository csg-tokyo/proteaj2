package phenan.prj

trait JTypeLoader {
  def compiler: JCompiler

  val arrayOf: JType => JArrayType

  def getObjectType (clazz: JClass, args: List[MetaValue]): Option[JObjectType]

  lazy val objectType = compiler.classLoader.objectClass.flatMap(_.objectType(Nil))
  lazy val voidType = compiler.classLoader.void.primitiveType

  def fromTypeSignature (sig: JTypeSignature, env: Map[String, MetaValue]): Option[JType]
  def fromClassTypeSignature (sig: JClassTypeSignature, env: Map[String, MetaValue]): Option[JObjectType]

  lazy val superTypesOfArray: List[JObjectType] = CommonNames.superClassesOfArray.flatMap { name =>
    compiler.classLoader.loadClass_PE(name).flatMap(_.objectType(Nil))
  }
}
