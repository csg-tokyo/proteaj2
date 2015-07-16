package phenan.prj.util

object LiteralUtil {
  def escape (c: Char): String = c match {
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case '\'' => "\\\'"
    case '\"' => "\\\""
    case '\\' => "\\\\"
    case x    => x.toString
  }
}