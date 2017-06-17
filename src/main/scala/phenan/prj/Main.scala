package phenan.prj

import phenan.prj.state.Config

/**
  * Created by ichikawa on 2017/06/17.
  */
object Main {
  def main (args: Array[String]): Unit = Config.configure(args).foreach { config =>
    JCompiler(config).compile()
  }
}
