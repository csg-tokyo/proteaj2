package phenan.prj

case class DSLInfo (priorities: List[String], constraints: List[List[JPriority]], withDSLs: List[JTypeSignature])

case class JPriority (clazz: JClassTypeSignature, name: String)
