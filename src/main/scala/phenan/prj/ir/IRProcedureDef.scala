package phenan.prj.ir

import phenan.prj._

sealed trait IRProcedureDef extends JMethodDef

class IRMethodDef extends IRProcedureDef {
  override def mod: JModifier = ???

  override def returnType: JErasedType = ???

  override def paramTypes: List[JErasedType] = ???

  override def declaringClass: JClass = ???

  override def name: String = ???

  override def exceptions: List[JClass] = ???
}

class IRConstructorDef extends IRProcedureDef {
  override def mod: JModifier = ???

  override def returnType: JErasedType = ???

  override def paramTypes: List[JErasedType] = ???

  override def declaringClass: JClass = ???

  override def name: String = ???

  override def exceptions: List[JClass] = ???
}

class IRInstanceInitializerDef extends IRProcedureDef {
  override def mod: JModifier = ???

  override def returnType: JErasedType = ???

  override def paramTypes: List[JErasedType] = ???

  override def declaringClass: JClass = ???

  override def name: String = ???

  override def exceptions: List[JClass] = ???
}

class IRStaticInitializerDef extends IRProcedureDef {
  override def mod: JModifier = ???

  override def returnType: JErasedType = ???

  override def paramTypes: List[JErasedType] = ???

  override def declaringClass: JClass = ???

  override def name: String = ???

  override def exceptions: List[JClass] = ???
}