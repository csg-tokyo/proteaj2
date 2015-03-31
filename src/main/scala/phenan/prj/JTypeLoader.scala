package phenan.prj

trait JTypeLoader {
  def compiler: JCompiler

  val arrayOf: JType => JArrayType
  val getObjectType: (JClass, List[MetaValue]) => Option[JObjectType]

  def fromTypeSignature (sig: JTypeSignature, env: Map[String, MetaValue]): Option[JType]
  def fromClassTypeSignature (sig: JClassTypeSignature, env: Map[String, MetaValue]): Option[JObjectType]

  lazy val superTypesOfArray: List[JObjectType] = CommonNames.superClassesOfArray.flatMap { name =>
    compiler.classLoader.loadClass_PE(name).flatMap(_.objectType(Nil))
  }
}
