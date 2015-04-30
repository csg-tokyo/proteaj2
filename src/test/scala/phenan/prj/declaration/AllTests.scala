package phenan.prj.declaration

import org.scalatest._

class AllTests extends Suites(new DeclarationParsersTest) {
  override def suiteName: String = "phenan.prj.declaration"
}
