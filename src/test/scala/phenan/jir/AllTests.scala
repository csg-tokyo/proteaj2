package phenan.jir

import org.scalatest._

class AllTests extends Suites(new JResolverTest, new JClassPathTest) {
  override def suiteName: String = "phenan.jir"
}
