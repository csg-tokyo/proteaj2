package phenan.jir

trait JMember {
  def modifier: JModifier

  def isPrivate: Boolean = modifier.check(JModifier.accPrivate)
}
