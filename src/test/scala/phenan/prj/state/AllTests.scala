package phenan.prj.state

import org.scalatest._

class AllTests extends Suites(new SearchPathTest) {
  override def suiteName: String = "phenan.prj.state"
}