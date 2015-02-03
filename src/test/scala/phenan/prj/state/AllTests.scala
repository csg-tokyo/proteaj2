package phenan.prj.state

import org.scalatest._

class AllTests extends Suites(new JSearchPathTest) {
  override def suiteName: String = "phenan.prj.state"
}