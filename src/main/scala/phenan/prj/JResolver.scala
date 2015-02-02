package phenan.prj

import phenan.prj.config.JConfig
import phenan.prj.internal.JClassLoader

trait JResolver {

}

object JResolver {

}

class JRootResolver (jdc: JDeclarationCompiler, config: JConfig) extends JResolver {
  private val loader = new JClassLoader(jdc, config)
}
