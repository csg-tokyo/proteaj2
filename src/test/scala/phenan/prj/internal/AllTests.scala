package phenan.prj.internal

import org.scalatest._

class AllTests extends Suites(new JClassLoaderTest) {
  override def suiteName: String = "phenan.prj.internal"
}
