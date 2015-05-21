package phenan.prj.ir

import org.scalatest._

class AllTests extends Suites(new NameResolverTest) {
  override def suiteName: String = "phenan.prj.ir"
}
