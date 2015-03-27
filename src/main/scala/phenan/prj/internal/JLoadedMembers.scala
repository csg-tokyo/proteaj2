package phenan.prj.internal

import phenan.prj._
import phenan.prj.exception._
import phenan.prj.state.JState

class JLoadedField (fieldDef: JLoadedFieldDef, val fieldType: JType, val declaring: JLoadedObjectType) extends JField {
  override def modifier: JModifier = fieldDef.mod
  override def name: String = fieldDef.name
}

trait JGenLoadedBehavior {
  def modifier: JModifier = methodDef.mod
  def name: String = methodDef.name

  lazy val returnType: JType =
    methodDef.signature.flatMap(sig => typePool.fromTypeSignature(sig.returnType, erasedEnv, loader)).orElse(typePool.toJValueType(methodDef.returnType)).getOrElse {
      this.state.error("cannot get the return type of method " + name)
      throw InvalidTypeException("cannot get the return type of method " + name)
    }

  lazy val parameterTypes: List[JType] = {
    import scalaz.Scalaz._
    methodDef.signature.flatMap(sig => sig.paramTypes.traverse(p => typePool.fromTypeSignature(p, erasedEnv, loader))).
      orElse(methodDef.paramTypes.traverse(typePool.toJValueType)).getOrElse {
      this.state.error("cannot get the parameter types of method " + name)
      throw InvalidTypeException("cannot get the parameter types of method " + name)
    }
  }

  lazy val exceptionTypes: List[JObjectType] = {
    import scalaz.Scalaz._
    methodDef.exceptions.traverse(_.objectType(Nil)).getOrElse {
      this.state.error("cannot get the exception types of method " + name)
      throw InvalidTypeException("cannot get the exception types of method " + name)
    }
  }

  def declaring: JLoadedObjectType
  protected def methodDef: JLoadedMethodDef
  protected def enclosingEnv: Map[String, MetaValue]
  protected implicit def state: JState

  private def loader = declaring.loader

  private lazy val erasedEnv: Map[String, MetaValue] = methodDef.signature match {
    case Some(sig) => typePool.rawTypeArguments(sig.metaParams, enclosingEnv, loader)
    case None      => enclosingEnv
  }

  private val typePool = JTypePool.get
}

class JGenLoadedInstanceMethod (val methodDef: JLoadedMethodDef, val enclosingEnv: Map[String, MetaValue], val declaring: JLoadedObjectType)(implicit val state: JState) extends JGenLoadedBehavior with JGenMethod {
  override def bind(typeArgs: List[MetaValue]): Option[JLoadedMethod] = ???

  override def infer(returnType: JType): Option[JLoadedMethod] = ???

  override def infer(argTypes: List[JType]): Option[JLoadedMethod] = ???
}

class JGenLoadedConstructor (val methodDef: JLoadedMethodDef, val enclosingEnv: Map[String, MetaValue], val declaring: JLoadedObjectType)(implicit val state: JState) extends JGenLoadedBehavior with JGenConstructor {
  
}


class JLoadedMethod extends JMethod {

}
