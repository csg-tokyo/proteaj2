package phenan.prj

import org.scalatest._

class AllTests extends Suites(new UnificationTest, new JCompilerTest, new state.AllTests, new internal.AllTests, new declaration.AllTests, new ir.AllTests, new body.AllTests) {
  override def suiteName: String = "phenan.prj"
}
