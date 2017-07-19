package phenan.prj.generator.sir2javarepr

import phenan.prj.JErasedTypes
import phenan.prj.generator.JavaRepr._
import phenan.prj.generator.SimplifiedIRs
import phenan.util._

/**
  * Created by ichikawa on 2017/07/14.
  */
trait JavaLiteralsGeneratorsModule {
  this: SimplifiedIRs with JErasedTypes =>

  object JavaLiteralsGenerators {
    def javaLiteral (literal: SIRJavaLiteral): JavaLiteral = literal match {
      case SIRNullLiteral       => Union[JavaLiteral](NullLiteral)
      case s: SIRStringLiteral  => Union[JavaLiteral](Literal(s.value))
      case c: SIRCharLiteral    => Union[JavaLiteral](Literal(c.value))
      case i: SIRIntLiteral     => Union[JavaLiteral](Literal(i.value))
      case j: SIRLongLiteral    => Union[JavaLiteral](Literal(j.value))
      case z: SIRBooleanLiteral => Union[JavaLiteral](Literal(z.value))
      case SIRObjectClassLiteral(clazz, d)        => Union[JavaLiteral](ClassLiteral(clazz.name, d))
      case SIRPrimitiveClassLiteral(primitive, d) => Union[JavaLiteral](ClassLiteral(primitive.name, d))
    }

    def classLiteral (clazz: JClass): ClassLiteral = ClassLiteral(clazz.name, 0)
  }
}
