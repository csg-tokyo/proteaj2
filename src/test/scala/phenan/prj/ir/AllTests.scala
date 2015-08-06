package phenan.prj.ir

import org.scalatest._

class AllTests extends Suites(new NameResolverTest, new EnvironmentTest) {
  override def suiteName: String = "phenan.prj.ir"
}
