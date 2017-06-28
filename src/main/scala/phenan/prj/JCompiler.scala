package phenan.prj

import java.io._

import phenan.prj.body._
import phenan.prj.declaration.DeclarationCompiler
import phenan.prj.exception.InitializationFailedException
import phenan.prj.generator._
import phenan.prj.internal._
import phenan.prj.ir._
import phenan.prj.signature._
import phenan.prj.state._

import scala.collection.mutable

trait JCompiler {
  this: IRs with DeclarationCompiler with JavaClassFileGenerator with Application =>

  def compile (): Unit = {
    for (file <- inputFiles; ir <- compileDeclaration(file)) {
      registerIR(ir)
    }
    generateClassFile(files.toList)
  }

  def compileFile(reader: Reader, file: String): Unit = {
    compileDeclaration(reader, file).foreach(registerIR)
  }

  def registerIR (ir: IRFile): Unit = {
    files += ir
    for (module <- ir.modules) modules += (module.internalName -> module)
  }

  def findIR (name: String): Option[IRModule] = modules.get(name)

  private val files: mutable.MutableList[IRFile] = mutable.MutableList.empty
  private var modules: Map[String, IRModule] = Map.empty
}

object JCompiler {
  case class JCompilerImpl (config: Config) extends JCompiler
    with BodyCompiler with BodyParser with DeclarationCompiler with JavaClassFileGenerator with JavaReprGenerator
    with StatementParsersModule with ExpressionParsersModule with ExpressionOperatorParsersModule with ExpressionOperandParsersModule
    with JavaExpressionParsersModule with ArgumentParsersModule with LiteralParsersModule with LiteralOperatorParsersModule
    with LiteralOperandParsersModule with JavaLiteralParsersModule with TypeParsersModule with CommonParsersModule with ContextSensitiveParsersModule
    with Unifier with ExpectedTypeInferencer with MethodContextInferencer with NameResolvers with OperatorPool
    with ClassFileLoader with ClassFileAnalyzer with ClassFileParser
    with JTypeLoader with JClassLoader with Environments with DSLEnvironments with FileEnvironments with EnvModifyStrategy
    with IRs with IRStatements with IRExpressions with IRAnnotationReader with SignatureParser with DescriptorParser
    with Syntax with JModules with JMembers with JErasedTypes with Application

  def init (config: Config): Either[InitializationFailedException, JCompilerImpl] = try {
    Right(JCompilerImpl(config))
  } catch { case e: InitializationFailedException => Left(e) }
}
