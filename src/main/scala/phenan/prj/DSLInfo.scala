package phenan.prj

case class DSLInfo (priorities: List[String], constraints: List[List[JPriority]], withDSLs: List[JClassTypeSignature])

case class JPriority (clazz: JClassTypeSignature, name: String)
