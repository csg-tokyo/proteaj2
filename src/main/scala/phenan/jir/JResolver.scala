package phenan.jir

import phenan.jir.internal.JClassLoader

trait JResolver {

}

object JResolver {

}

class JRootResolver (implicit config: JirConfig) extends JResolver {
  private val loader = new JClassLoader()
}
