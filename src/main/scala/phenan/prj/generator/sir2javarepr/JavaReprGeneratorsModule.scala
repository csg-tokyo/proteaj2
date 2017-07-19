package phenan.prj.generator.sir2javarepr

import phenan.prj._
import phenan.prj.generator.JavaRepr._
import phenan.prj.generator.SimplifiedIRs
import phenan.util._

/**
  * Created by ichikawa on 2017/07/13.
  */
trait JavaReprGeneratorsModule {
  this: JavaStatementGeneratorsModule with JavaExpressionGeneratorsModule with JavaAnnotationsGeneratorsModule
    with JavaSignatureGeneratorsModule with SimplifiedIRs =>

  object JavaReprGenerators {
    def javaFile (file: SIRFile): JavaFile = JavaFile(file.packageName, file.modules.map(moduleDef))

    def moduleDef (clazz: SIRModule): ModuleDef = clazz match {
      case cls: SIRClass     => Union[ModuleDef](classDef(cls))
      case enm: SIREnum      => Union[ModuleDef](enumDef(enm))
      case ifc: SIRInterface => Union[ModuleDef](interfaceDef(ifc))
    }

    private def classDef (clazz: SIRClass): ClassDef = new ClassDef {
      def annotations: List[JavaAnnotation] = JavaAnnotationsGenerators.classAnnotations(clazz)
      def modifiers: JModifier = clazz.modifiers ^ JModifier.accSuper
      def name: String = clazz.name
      def typeParameters: List[TypeParam] = JavaSignatureGenerators.typeParams(clazz.metaParams)
      def superType: ClassSig = JavaSignatureGenerators.classSig(clazz.superClass)
      def interfaces: List[ClassSig] = clazz.interfaces.map(JavaSignatureGenerators.classSig)
      def members: List[ClassMember] = clazz.members.map(memberDef)
    }

    private def enumDef (enum: SIREnum): EnumDef = new EnumDef {
      def annotations: List[JavaAnnotation] = JavaAnnotationsGenerators.enumAnnotations(enum)
      def modifiers: JModifier = enum.modifiers ^ JModifier.accSuper
      def name: String = enum.name
      def interfaces: List[ClassSig] = enum.interfaces.map(JavaSignatureGenerators.classSig)
      def constants: List[EnumConstantDef] = enum.constants.map(enumConstantDef)
      def members: List[ClassMember] = enum.members.map(memberDef)
    }

    private def interfaceDef (interface: SIRInterface): InterfaceDef = new InterfaceDef {
      def annotations: List[JavaAnnotation] = JavaAnnotationsGenerators.interfaceAnnotations(interface)
      def modifiers: JModifier = interface.modifiers
      def name: String = interface.name
      def typeParameters: List[TypeParam] = JavaSignatureGenerators.typeParams(interface.metaParams)
      def superInterfaces: List[ClassSig] = interface.interfaces.map(JavaSignatureGenerators.classSig)
      def members: List[ClassMember] = interface.members.map(memberDef)
    }

    private def memberDef (member: SIRMember): ClassMember = member match {
      case field: SIRField             => Union[ClassMember](fieldDef(field))
      case method: SIRMethod           => Union[ClassMember](methodDef(method))
      case constructor: SIRConstructor => Union[ClassMember](constructorDef(constructor))
      case iin: SIRInstanceInitializer => Union[ClassMember](instanceInitializerDef(iin))
      case sin: SIRStaticInitializer   => Union[ClassMember](staticInitializerDef(sin))
      case module: SIRModule           => Union[ClassMember](moduleDef(module))
    }

    private def fieldDef (field: SIRField): FieldDef = new FieldDef {
      def annotations: List[JavaAnnotation] = JavaAnnotationsGenerators.fieldAnnotations(field)
      def modifiers: JModifier = field.modifiers
      def fieldType: TypeSig = JavaSignatureGenerators.typeSig(field.signature)
      def name: String = field.name
      def initializer: Option[Expression] = field.initializer.map(JavaExpressionGenerators.expression)
    }

    private def methodDef (method: SIRMethod): MethodDef = new MethodDef {
      def annotations: List[JavaAnnotation] = JavaAnnotationsGenerators.methodAnnotations(method)
      def modifiers: JModifier = method.modifiers
      def typeParameters: List[TypeParam] = JavaSignatureGenerators.typeParams(method.metaParams)
      def returnType: TypeSig = JavaSignatureGenerators.typeSig(method.javaReturnType)
      def name: String = method.name
      def parameters: List[Param] = method.javaParameters.map(parameter)
      def throws: List[TypeSig] = method.throwTypes.map(JavaSignatureGenerators.typeSig)
      def body: Option[Block] = method.body.map(JavaStatementGenerators.block)
    }

    private def constructorDef (constructor: SIRConstructor): ConstructorDef = new ConstructorDef {
      def annotations: List[JavaAnnotation] = JavaAnnotationsGenerators.constructorAnnotations(constructor)
      def modifiers: JModifier = constructor.modifiers
      def typeParameters: List[TypeParam] = JavaSignatureGenerators.typeParams(constructor.metaParams)
      def className: String = constructor.className
      def parameters: List[Param] = constructor.javaParameters.map(parameter)
      def throws: List[TypeSig] = constructor.throwTypes.map(JavaSignatureGenerators.typeSig)
      def body: Block = JavaStatementGenerators.block(constructor.body)
    }

    private def instanceInitializerDef (iin: SIRInstanceInitializer): InstanceInitializerDef = {
      InstanceInitializerDef(JavaStatementGenerators.block(iin.body))
    }

    private def staticInitializerDef (sin: SIRStaticInitializer): StaticInitializerDef = {
      StaticInitializerDef(JavaStatementGenerators.block(sin.body))
    }

    private def enumConstantDef (constant: SIREnumConstant): EnumConstantDef = EnumConstantDef(constant.name)

    private def parameter (param: SIRFormalParameter): Param = Param (JavaSignatureGenerators.typeSig(param.paramType), param.name)
  }
}

