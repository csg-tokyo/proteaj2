package phenan.prj

import phenan.prj.state._
import phenan.prj.internal.JClassLoader

trait JResolver {

}

object JResolver {

}

class JRootResolver (jdc: JCompiler)(implicit state: JState) extends JResolver {
  private val loader = new JClassLoader(jdc)
}
