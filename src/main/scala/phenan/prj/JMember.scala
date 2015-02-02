package phenan.prj

trait JMember {
  def modifier: JModifier

  def isPrivate: Boolean = modifier.check(JModifier.accPrivate)
}
