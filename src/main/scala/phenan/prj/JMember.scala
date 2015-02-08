package phenan.prj

trait JMember {
  def modifier: JModifier
  def declaring: JType

  def isPrivate: Boolean = modifier.check(JModifier.accPrivate)
}
