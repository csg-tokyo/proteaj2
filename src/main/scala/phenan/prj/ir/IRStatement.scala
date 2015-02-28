package phenan.prj.ir

sealed trait IRStatement

case class IRBlock (statements: List[IRStatement]) extends IRStatement
