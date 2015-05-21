package phenan.prj.body

import org.scalatest._

class AllTests extends Suites(new TypeParsersTest) {
  override def suiteName: String = "phenan.prj.body"
}