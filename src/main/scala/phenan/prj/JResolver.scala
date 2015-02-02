package phenan.prj

import phenan.prj.internal.JClassLoader

trait JResolver {

}

object JResolver {

}

class JRootResolver (implicit config: JirConfig) extends JResolver {
  private val loader = new JClassLoader()
}
