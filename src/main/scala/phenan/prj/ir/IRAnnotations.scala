package phenan.prj.ir

import phenan.prj._
import phenan.prj.declaration._

import scalaz._

class IRAnnotationReader (resolver: NameResolver) {
  def annotation [T] (clazz: JClass)(reader: Reader[Annotation, T]): Reader[List[Annotation], Option[T]] = annotationReader.map(_.get(clazz).map(reader =<< _))

  lazy val annotationReader: Reader[List[Annotation], Map[JClass, Annotation]] =
    Reader(_.flatMap { a => resolver.resolve(a.name.names).map(_ -> a).toOption }.toMap)

  def enumSwitch [T] (name: String, enumType: JType)(readers: PartialFunction[String, Reader[Annotation, Option[T]]]): Reader[Annotation, Option[T]] = element(name).map(_.flatMap(expression(enumType) =<< _)).flatMap {
    case Some(ConcretePureValue(en: java.lang.Enum[_], _)) => readers.applyOrElse(en.name(), { _: String => Reader(_ => None) })
    case _ => Reader(_ => None)
  }

  def array [T] (name: String)(reader: Reader[AnnotationElement, Option[T]]): Reader[Annotation, List[T]] = element(name).map {
    case Some(ArrayOfAnnotationElement(array)) => array.flatMap(reader =<< _)
    case _ => Nil
  }

  def elementAnnotation [T] (name: String)(clazz: JClass)(reader: Reader[Annotation, T]): Reader[Annotation, Option[T]] = element(name).map(_.collect {
    case ann: Annotation if resolver.resolve(ann.name.names).toOption.contains(clazz) => reader =<< ann
  })

  def element (name: String): Reader[Annotation, Option[AnnotationElement]] = Reader {
    case MarkerAnnotation(_)           => None
    case SingleElementAnnotation(_, arg) if name == "value" => Some(arg)
    case SingleElementAnnotation(_, _) => None
    case FullAnnotation(_, args)       => args.get(name)
  }

  lazy val string: Reader[AnnotationElement, Option[String]] = compiler.typeLoader.stringType.map(s => expression(s).map {
    case Some(ConcretePureValue(str: java.lang.String, _)) => Some(str)
    case _ => None
  }).getOrElse(Reader(_ => None))

  lazy val clazz: Reader[AnnotationElement, Option[JErasedType]] = compiler.typeLoader.anyClassType.map(c => expression(c).map {
    case Some(ConcretePureValue(cls: java.lang.Class[_], _)) => compiler.classLoader.load(cls.getName).toOption
    case _ => None
  }).getOrElse(Reader(_ => None))

  def expression (expected: JType): Reader[AnnotationElement, Option[MetaValue]] = Reader {
    case ExpressionSnippet(snippet) => compiler.bodyCompiler.expression(snippet, expected, BaseEnvironment(resolver)).flatMap(_.eval).toOption
    case _ => None
  }

  def compiler = resolver.root.compiler
}




// TODO:
@Deprecated
trait IRAnnotations {
  def resolver: NameResolver
  def annotations: List[Annotation]

  def classSig: Option[JClassSignature] = find (JTypeSignature.classSigTypeSig).map { args =>
    val metaParams = args.get("metaParameters")
    val superType = args.get("superType")
    val interfaces = args.get("interfaces")
    ???
  }

  def find (sig: JClassTypeSignature): Option[Map[String, AnnotationElement]] = annotations.collectFirst {
    case MarkerAnnotation(name) if resolver.classTypeSignature(name).toOption.contains(sig) => Map.empty
    case SingleElementAnnotation(name, arg) if resolver.classTypeSignature(name).toOption.contains(sig) => Map("value" -> arg)
    case FullAnnotation(name, args) if resolver.classTypeSignature(name).toOption.contains(sig) => args
  }

  type ElemReader[A] = scalaz.ReaderT[Option, AnnotationElement, A]
  object ElemReader extends scalaz.KleisliInstances with scalaz.KleisliFunctions {
    def apply[A] (f: AnnotationElement => Option[A]): ElemReader[A] = kleisli(f)
  }

}
