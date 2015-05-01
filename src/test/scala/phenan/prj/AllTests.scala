package phenan.prj

import org.scalatest._

class AllTests extends Suites(new UnificationTest, new JCompilerTest, new state.AllTests, new internal.AllTests, new declaration.AllTests) {
  override def suiteName: String = "phenan.prj"
}
