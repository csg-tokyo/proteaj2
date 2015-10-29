package phenan.prj

import org.scalatest._

class AllTests extends Suites(new UnificationTest, new state.AllTests, new internal.AllTests, new declaration.AllTests, new ir.AllTests, new body.AllTests, new JCompilerTest) {
  override def suiteName: String = "phenan.prj"
}
