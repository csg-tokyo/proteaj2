package phenan.prj

import scala.util.Try

trait JMethod extends JMember {
  def name: String

  def overrides (method: JMethod): Boolean

  def bind (typeArgs: List[JValueType]): Try[JParameterizedMethod]
  def infer (returnType: JValueType): Try[JParameterizedMethod]
  def infer (argTypes: List[JValueType]): Try[JParameterizedMethod]
}

trait JParameterizedMethod {

}
