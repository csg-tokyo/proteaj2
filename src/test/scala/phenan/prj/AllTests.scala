package phenan.prj

import org.scalatest._

class AllTests extends Suites(new JResolverTest, new config.AllTests, new internal.AllTests) {
  override def suiteName: String = "phenan.jir"
}
