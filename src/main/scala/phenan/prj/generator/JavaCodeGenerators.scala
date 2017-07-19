package phenan.prj.generator

import phenan.prj._
import phenan.prj.util._

import JavaRepr._

object JavaCodeGenerators extends Generators {
  lazy val javaFile: Generator[JavaFile] = ( "package" ~> string <~ ';' ).? ~ moduleDef.* ^^ { file =>
    file.packageName -> file.modules
  }

  lazy val moduleDef: Generator[ModuleDef] = classDef :|: enumDef :|: interfaceDef :|: nil

  lazy val classDef: Generator[ClassDef] = annotations ~ modifier ~ ( "class" ~> string ) ~ typeParam.*?('<', ',', '>') ~ ( "extends" ~> classSig ) ~ classSig.*?("implements", ',', "") ~ ( '{' ~> indent(classMember.*(newLine)) <~ '}' ) ^^ { clazz =>
    clazz.annotations -> clazz.modifiers -> clazz.name -> clazz.typeParameters -> clazz.superType -> clazz.interfaces -> clazz.members
  }

  lazy val enumDef: Generator[EnumDef] = annotations ~ modifier ~ ( "enum" ~> string ) ~ classSig.*?("implements", ',', "") ~ ( '{' ~> indent(enumConstantDef.*(',') ~ classMember.*?(';' ~> newLine, newLine, "")) <~ '}' ) ^^ { enum =>
    enum.annotations -> enum.modifiers -> enum.name -> enum.interfaces -> ( enum.constants -> enum.members )
  }

  lazy val interfaceDef: Generator[InterfaceDef] = annotations ~ modifier ~ ( "interface" ~> string ) ~ typeParam.*?('<', ',', '>') ~ classSig.*?("extends", ',', "") ~ ( '{' ~> indent(classMember.*(newLine)) <~ '}' ) ^^ { interface =>
    interface.annotations -> interface.modifiers -> interface.name -> interface.typeParameters -> interface.superInterfaces -> interface.members
  }

  lazy val classMember: Generator[ClassMember] = fieldDef :|: methodDef :|: constructorDef :|: instanceInitializerDef :|: staticInitializerDef :|: moduleDef :|: nil

  lazy val fieldDef: Generator[FieldDef] = annotations ~ modifier ~ typeSig ~ string ~ ( "=" ~> expression ).? <~ ';' ^^ { field =>
    field.annotations -> field.modifiers -> field.fieldType -> field.name -> field.initializer
  }

  lazy val methodDef: Generator[MethodDef] = annotations ~ modifier ~ typeParam.*?('<', ',', '>') ~ typeSig ~ string ~ ( '(' ~> parameter.*(',') <~ ')' ) ~ typeSig.*?("throws", ',', "") ~ block.?(';') ^^ { method =>
    method.annotations -> method.modifiers -> method.typeParameters -> method.returnType -> method.name -> method.parameters -> method.throws -> method.body
  }

  lazy val constructorDef: Generator[ConstructorDef] = annotations ~ modifier ~ typeParam.*?('<', ',', '>') ~ string ~ ( '(' ~> parameter.*(',') <~ ')' ) ~ typeSig.*?("throws", ',', "") ~ block ^^ { constructor =>
    constructor.annotations -> constructor.modifiers -> constructor.typeParameters -> constructor.className -> constructor.parameters -> constructor.throws -> constructor.body
  }

  lazy val instanceInitializerDef: Generator[InstanceInitializerDef] = block ^^ { _.body }

  lazy val staticInitializerDef: Generator[StaticInitializerDef] = "static" ~> block ^^ { _.body }

  lazy val enumConstantDef: Generator[EnumConstantDef] = string ^^ { _.name }

  lazy val parameter: Generator[Param] = typeSig ~ ( ␣ ~> string ) ^^ { param =>
    param.parameterType -> param.name
  }

  lazy val statement: Generator[Statement] = block :|: localDeclarationStatement :|: ifStatement :|: whileStatement :|: enhancedForStatement :|: tryStatement :|: throwStatement :|: returnStatement :|: expressionStatement :|: breakStatement :|: thisConstructorCall :|: superConstructorCall :|: nil

  lazy val block: Generator[Block] = '{' ~> indent(statement.*(newLine)) <~ '}' ^^ { _.statements }

  lazy val localDeclarationStatement: Generator[LocalDeclaration] = localDeclaration <~ ';'

  lazy val localDeclaration: Generator[LocalDeclaration] = typeSig ~ string ~ ( '=' ~> expression ).? ^^ { local =>
    local.localType -> local.name -> local.initializer
  }

  lazy val ifStatement: Generator[IfStatement] = "if" ~> ( '(' ~> expression <~ ')' ) ~ statement ~ ( "else" ~> statement ).? ^^ { stmt =>
    stmt.condition -> stmt.thenStatement -> stmt.elseStatement
  }

  lazy val whileStatement: Generator[WhileStatement] = "while" ~> ( '(' ~> expression <~ ')' ) ~ statement ^^ { stmt =>
    stmt.condition -> stmt.loopBody
  }

  lazy val enhancedForStatement: Generator[EnhancedForStatement] = "for" ~> ( '(' ~> typeSig ~ string ~ ( ':' ~> expression ) <~ ')' ) ~ statement ^^ { stmt =>
    stmt.elementType -> stmt.name -> stmt.iterable -> stmt.loopBody
  }

  lazy val tryStatement: Generator[TryStatement] = ( "try" ~> block ) ~ catchClause.* ~ ( "finally" ~> block ).? ^^ { stmt =>
    stmt.tryBlock -> stmt.catchBlocks -> stmt.finallyBlock
  }

  lazy val catchClause: Generator[ExceptionHandler] = "catch" ~> ( '(' ~> typeSig ~ string <~ ')' ) ~ block ^^ { clause =>
    clause.exceptionType -> clause.name -> clause.block
  }

  lazy val throwStatement: Generator[ThrowStatement] = "throw" ~> expression <~ ';' ^^ { _.exception }

  lazy val returnStatement: Generator[ReturnStatement] = "return" ~> expression.? <~ ';' ^^ { _.returnValue }

  lazy val expressionStatement: Generator[ExpressionStatement] = expression <~ ';' ^^ { _.statementExpression }

  lazy val breakStatement: Generator[BreakStatement.type] = "break" <~ ';'  ^^ { _ => () }

  lazy val thisConstructorCall: Generator[ThisConstructorCall] = typeArg.*?('<', ',', '>') ~ ( "this" ~> '(' ~> expression.*(',') <~ ')' ) ^^ { cc =>
    cc.typeArguments -> cc.arguments
  }

  lazy val superConstructorCall: Generator[SuperConstructorCall] = typeArg.*?('<', ',', '>') ~ ( "super" ~> '(' ~> expression.*(',') <~ ')' ) ^^ { cc =>
    cc.typeArguments -> cc.arguments
  }

  lazy val expression: Generator[Expression] = assignment :|: methodCall :|: fieldAccess :|: castExpression :|: arrayAccess :|: newExpression :|: anonymousClass :|: lambda :|: newArray :|: arrayInit :|: localRef :|: thisRef :|: javaLiteral :|: nil

  lazy val receiver: Generator[Receiver] = expression :|: classRef :|: superRef :|: nil

  lazy val assignment: Generator[Assignment] = expression ~ ( '=' ~> expression ) ^^ { assign =>
    assign.left -> assign.right
  }

  lazy val methodCall: Generator[MethodCall] = ( receiver <~ '.' ) ~ typeArg.*?('<', ',', '>') ~ string ~ ( '(' ~> expression.*(',') <~ ')' ) ^^ { m =>
    m.receiver -> m.typeArguments -> m.methodName -> m.arguments
  }

  lazy val fieldAccess: Generator[FieldAccess] = ( receiver <~ '.' ) ~ string ^^ { f =>
    f.receiver -> f.fieldName
  }

  lazy val castExpression: Generator[CastExpression] = ( '(' ~> typeSig <~ ')' ) ~ ( '(' ~> expression <~ ')' ) ^^ { cast =>
    cast.destType -> cast.castedExpression
  }

  lazy val arrayAccess: Generator[ArrayAccess] = expression ~ ( '[' ~> expression <~ ']' ) ^^ { expr =>
    expr.array -> expr.index
  }

  lazy val newExpression: Generator[NewExpression] = "new" ~> typeArg.*?('<', ',', '>') ~ classSig ~ ( '(' ~> expression.*(',') <~ ')' ) ^^ { expr =>
    expr.typeArguments -> expr.constructType -> expr.arguments
  }

  lazy val anonymousClass: Generator[AnonymousClass] = "new" ~> classSig ~ ( '(' ~> expression.*(',') <~ ')' ) ~ ( '{' ~> indent(classMember.*(newLine)) <~ '}' ) ^^ { expr =>
    expr.baseType -> expr.arguments -> expr.members
  }

  lazy val lambda: Generator[Lambda] = ( '(' ~> parameter.*(',') <~ ')' ) ~ ( "->" ~> block ) ^^ { lambda =>
    lambda.parameters -> lambda.body
  }

  lazy val newArray: Generator[NewArray] = "new" ~> typeSig ~ ( '[' ~> expression <~ ']' ).* ~ dimension ^^ { expr =>
    expr.componentType -> expr.arraySize -> expr.dim
  }

  lazy val arrayInit: Generator[ArrayInit] = "new" ~> typeSig ~ dimension ~ ( '{' ~> expression.*(',') <~ '}' ) ^^ { expr =>
    expr.componentType -> expr.dim -> expr.components
  }

  lazy val localRef: Generator[LocalRef] = elem { _.name }

  lazy val thisRef: Generator[ThisRef] = classRef <~ '.' <~ "this" ^^ { _.thisClass }

  lazy val classRef: Generator[ClassRef] = elem { _.name }

  lazy val superRef: Generator[SuperRef] = classRef <~ '.' <~ "super" ^^ { _.thisClass }

  lazy val javaLiteral: Generator[JavaLiteral] = nullLiteral :|: classLiteral :|: stringLiteral :|: charLiteral :|: intLiteral :|: longLiteral :|: booleanLiteral :|: nil

  lazy val nullLiteral: Generator[NullLiteral.type] = elem { _ => "null" }

  lazy val classLiteral: Generator[ClassLiteral] = string ~ dimension <~ '.' <~ "class" ^^ { lit =>
    lit.className -> lit.dim
  }

  lazy val dimension: Generator[Int] = elem { dim => mul("[]", dim) }

  lazy val stringLiteral: Generator[Literal[String]] = '\"' ~> literalChar.* <~ '\"' ^^ { _.value.toList }

  lazy val charLiteral: Generator[Literal[Char]] = '\'' ~> literalChar <~ '\'' ^^ { _.value }

  lazy val intLiteral: Generator[Literal[Int]] = elem { _.value.toString }

  lazy val longLiteral: Generator[Literal[Long]] = elem { _.value.toString + 'L' }

  lazy val booleanLiteral: Generator[Literal[Boolean]] = elem { _.value.toString }

  lazy val literalChar: Generator[Char] = lexical { LiteralUtil.escape }


  lazy val typeParam: Generator[TypeParam] = string ~ typeSig.*?("extends", "&", "") ^^ { param =>
    param.name -> param.bounds
  }

  lazy val typeArg: Generator[TypeArg] = typeSig :|: wildcard :|: nil

  lazy val typeSig: Generator[TypeSig] = classSig :|: arraySig :|: typeVariableSig :|: primitiveSig :|: nil

  lazy val classSig: Generator[ClassSig] = topLevelClassSig :|: memberClassSig :|: nil

  lazy val topLevelClassSig: Generator[TopLevelClassSig] = string ~ typeArg.*?('<', ',', '>') ^^ { sig =>
    sig.className -> sig.typeArguments
  }

  lazy val memberClassSig: Generator[MemberClassSig] = classSig ~ ( '.' ~> string ) ~ typeArg.*?('<', ',', '>') ^^ { sig =>
    sig.outer -> sig.className -> sig.typeArguments
  }

  lazy val arraySig: Generator[ArraySig] = typeSig <~ '[' <~ ']' ^^ { _.component }

  lazy val typeVariableSig: Generator[TypeVariableSig] = elem { _.name }

  lazy val primitiveSig: Generator[PrimitiveSig] = elem { _.name }

  lazy val wildcard: Generator[Wildcard] = unboundWildcard :|: upperBoundWildcard :|: lowerBoundWildcard :|: nil

  lazy val unboundWildcard: Generator[UnboundWildcard.type] = unit('?') ^^ { _ => () }

  lazy val upperBoundWildcard: Generator[UpperBoundWildcard] = '?' ~> "extends" ~> typeSig ^^ { _.bound }

  lazy val lowerBoundWildcard: Generator[LowerBoundWildcard] = '?' ~> "super" ~> typeSig ^^ { _.bound }

  lazy val modifier: Generator[JModifier] = elem { _.toString }

  lazy val annotations: Generator[List[JavaAnnotation]] = annotation.*?("", "", newLine)

  lazy val annotation: Generator[JavaAnnotation] = ( '@' ~> string ) ~ annotationArgument.*?('(', ',', ')') ^^ { ann =>
    ann.name -> ann.arguments.toList
  }

  lazy val annotationArgument: Generator[(String, AnnotationElement)] = string ~ ( '=' ~> annotationElement )

  lazy val annotationElement: Generator[AnnotationElement] = elementArray :|: annotation :|: javaLiteral :|: enumConstRef :|: nil

  lazy val elementArray: Generator[ElementArray] = '{' ~> annotationElement.*(',') <~ '}' ^^ { _.elements }

  lazy val enumConstRef: Generator[EnumConstRef] = elem { e =>
    e.enumName + '.' + e.constantName
  }

  def mul (s: String, n: Int): String = (0 until n).map(_ => s).mkString

  val spacingBeforeWord: List[Char] = List('?', '@', '{')

  val spacingAfterWord: List[Char] = List(',', ')', '}', '>')
}
