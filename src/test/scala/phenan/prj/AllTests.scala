package phenan.prj

import org.scalatest._

class AllTests extends Suites(new JResolverTest, new JSearchPathTest) {
  override def suiteName: String = "phenan.jir"
}
