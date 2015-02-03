package phenan.prj

import org.scalatest._

class AllTests extends Suites(new JResolverTest, new state.AllTests, new internal.AllTests) {
  override def suiteName: String = "phenan.prj"
}
