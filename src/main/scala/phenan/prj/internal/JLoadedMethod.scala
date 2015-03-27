package phenan.prj.internal

import phenan.prj._
import phenan.prj.exception._
import phenan.prj.state.JState

class JLoadedObjectGenMethod (val methodDef: JLoadedMethodDef, val signature: Option[MethodSignature], val declaring: JLoadedObjectType, val typeArguments: Map[String, MetaValue])(implicit st: JState) extends JGenMethod {
  override def modifier: JModifier = methodDef.mod
  
  override def name: String = methodDef.name

  lazy val returnType: JType =
    signature.flatMap(sig => typePool.fromTypeSignature(sig.returnType, env, loader)).orElse(typePool.toJValueType(methodDef.returnType)).getOrElse {
      st.error("cannot get the return type of method " + name)
      throw InvalidTypeException("cannot get the return type of method " + name)
    }

  lazy val parameterTypes: List[JType] = {
    import scalaz.Scalaz._
    signature.flatMap(sig => sig.paramTypes.traverse(p => typePool.fromTypeSignature(p, env, loader))).
      orElse(methodDef.paramTypes.traverse(typePool.toJValueType)).getOrElse {
      st.error("cannot get the parameter types of method " + name)
      throw InvalidTypeException("cannot get the parameter types of method " + name)
    }
  }

  lazy val exceptionTypes: List[JObjectType] = {
    import scalaz.Scalaz._
    methodDef.exceptions.traverse(_.objectType(Nil)).getOrElse {
      st.error("cannot get the exception types of method " + name)
      throw InvalidTypeException("cannot get the exception types of method " + name)
    }
  }


  override def bind(typeArgs: List[JType]): Option[JLoadedMethod] = ???

  override def infer(returnType: JType): Option[JLoadedMethod] = ???

  override def infer(argTypes: List[JType]): Option[JLoadedMethod] = ???

  def loader = declaring.loader

  lazy val env: Map[String, MetaValue] = signature match {
    case Some(sig) => typePool.rawTypeArguments(sig.metaParams, typeArguments, loader)
    case None      => typeArguments
  }

  val typePool = JTypePool.get
}

class JLoadedMethod extends JMethod {

}
