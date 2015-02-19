package phenan.prj.decl

import org.scalatest._

class AllTests extends Suites(new DeclarationParserTest, new SourceReaderTest) {
  override def suiteName: String = "phenan.prj.decl"
}
