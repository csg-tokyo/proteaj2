package phenan.prj.config

import org.scalatest._

class AllTests extends Suites(new JSearchPathTest) {
  override def suiteName: String = "phenan.jir.config"
}