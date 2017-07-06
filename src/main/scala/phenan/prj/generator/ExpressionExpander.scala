package phenan.prj.generator

import phenan.prj._
import phenan.prj.ir._

import scalaz._

import scalaz.syntax.traverse._
import scalaz.std.list._

/**
  * Created by ichikawa on 2017/07/04.
  */
trait ExpressionExpander {
  this: SimplifiedIRs with IRStatements with IRExpressions with JModules with JErasedTypes with Application =>

  object SimplifiedIRGenerators {
    private type Gen[T] = RWS[Unit, List[SIRStatement], Map[JObjectType, SIRLocalRef], T]

    //private def stateT [T] (f: Map[IRContextRef, SIRLocalRef] => Writer[List[SIRStatement], (Map[IRContextRef, SIRLocalRef], T)]): Gen[T] = kleisli[Writer[List[SIRStatement], ?], Map[IRContextRef, SIRLocalRef], T](f)

    private def tell (stmt: SIRStatement): Gen[Unit] = WriterT.tell[List[SIRStatement]](List(stmt)).rwst

    private def tell (stmts: List[SIRStatement]): Gen[Unit] = WriterT.tell[List[SIRStatement]](stmts).rwst

    private def modify (f: Map[JObjectType, SIRLocalRef] => Map[JObjectType, SIRLocalRef]): Gen[Unit] = State.modify(f).rwst

    //private def reader [T] (f: Map[IRContextRef, SIRLocalRef] => T): Gen[T] = readerT(a => Writer(Nil, f(a)))

    private def ret [T] (t: T): Gen[T] = RWS((_, s) => (Nil, t, s))

    //private implicit def genMonad: Monad[Gen] = ReaderT.kleisliMonadReader[Writer[List[SIRStatement], ?], Map[IRContextRef, SIRLocalRef]](WriterT.writerTMonad[Id.Id, List[SIRStatement]])

    def statement (stmt: IRStatement): Gen[Unit] = stmt match {
      case IRLocalDeclarationStatement(declaration) => localDeclaration(declaration).flatMap(tell)
      case IRBlock(stmts) => stmts.traverse_[Gen](statement)
      case ifStmt : IRIfStatement => ???
      case forStmt : IRForStatement => ???
      case whileStmt : IRWhileStatement => ???
      case tryStmt : IRTryStatement => ???
      case throwStmt : IRThrowStatement => ???
      case activateStmt : IRActivateStatement => ???
      case returnStmt : IRReturnStatement => ???
      case exprStmt : IRExpressionStatement => ???
    }

    def localDeclaration (local: IRLocalDeclaration): Gen[List[SIRLocalDeclaration]] = {
      val IRLocalDeclaration(localType, declarators) = local
      declarators.traverse[Gen, SIRLocalDeclaration] { case IRVariableDeclarator(name, dim, init) =>
        init.map(expression) match {
          case Some(gen) =>
            gen.map { expr =>
              SIRLocalDeclaration(localType.array(dim), name, Some(expr))
            }
          case None =>
            ret(SIRLocalDeclaration(localType.array(dim), name, None))
        }
      }
    }

    def expression (expr: IRExpression): Gen[SIRSimpleExpression] = expr match {
      case IRStatementExpression(stmt) => statement(stmt).map(_ => SIRNullLiteral)

      case scopeArg : IRScopeArgument => scopeArgument(scopeArg)
      case contextArg: IRContextualArgument => contextSensitiveArgument(contextArg)

      case variableArg : IRVariableArguments => ???
      case defaultArg : IRDefaultArgument => ???
      case assignment : IRAssignmentExpression => ???

      case methodCall : IRMethodCall => ???
      case constructorCall : IRNewExpression => ???
      case fieldAccess : IRFieldAccess => ???

      case arrayNew : IRArrayCreation => ???
      case arrayAccess : IRArrayAccess => ???

      case localRef : IRLocalVariableRef => ???
      case contextRef : IRContextRef => ???
      case thisRef : IRThisRef => ???

      case castExpr : IRCastExpression => ???

      case literal : IRJavaLiteral => ret(javaLiteral(literal))
      case anonClass : IRAnonymousClass => ???
    }

    /* foo(bar(baz(), hoge()))
     *     ~~~~~~~~~~~~~~~~~~
     *
     * Context c;
     * Baz x = baz();
     * c = ActivatedContext.get(0);
     * Hoge y = c.hoge();
     * Bar z = bar(x, y);
     * Foo w = foo(z)
     * ---
     * w
     */
    def scopeArgument (scopeArg : IRScopeArgument): Gen[SIRSimpleExpression] = {
      val IRScopeArgument(arg, scopes) = scopeArg
      val cs = scopes.map(c => c.contextType -> generateLocalName)

      for {
        _ <- tell(cs.map { case (typ, name) => SIRLocalDeclaration(typ, name, None) })
        _ <- modify(locals => locals ++ cs.map { case (typ, name) => (typ, SIRLocalRef(name)) })
        e <- expression(arg)
      } yield e
    }

    /* foo(bar(baz(), hoge()))
     *     ~~~~~~~~~~~~~~~~~~
     *
     * Foo w = foo((Context c) -> {
     *   Baz x = c.baz();
     *   Hoge y = c.hoge();
     *   Bar z = bar(x, y);
     *   return z;
     * })
     * ---
     * w
     */
    def contextSensitiveArgument (csArg: IRContextualArgument): Gen[SIRSimpleExpression] = RWS { (r, s) =>
      val IRContextualArgument(arg, contexts) = csArg
      val cs = contexts.map(c => c.contextType -> generateLocalName)
      val s2 = s ++ cs.map { case (typ, str) => (typ, SIRLocalRef(str)) }
      val (body, e, _) = expression(arg).run(r, s2)
      val csExpr = SIRContextSensitiveExpression(cs, body :+ SIRReturnStatement(e))
      val generatedName = generateLocalName
      val localDecl = SIRLocalDeclaration(csArg.staticType, generatedName, Some(csExpr))
      (List(localDecl), SIRLocalRef(generatedName), s)
    }

    def variableArgument (vArgs: IRVariableArguments): Gen[SIRExpression] = vArgs.componentType match {
      case p: JPrimitiveType => vArgs.args.traverse(expression).map(???)
      case r: JRefType       => ???
    }

    def javaLiteral (literal: IRJavaLiteral): SIRJavaLiteral = literal match {
      case IRObjectClassLiteral(clazz, dim)  => SIRObjectClassLiteral(clazz, dim)
      case IRPrimitiveClassLiteral(prm, dim) => SIRPrimitiveClassLiteral(prm, dim)
      case IRCharLiteral(value)    => SIRCharLiteral(value)
      case IRIntLiteral(value)     => SIRIntLiteral(value)
      case IRLongLiteral(value)    => SIRLongLiteral(value)
      case IRBooleanLiteral(value) => SIRBooleanLiteral(value)
      case IRStringLiteral(value)  => SIRStringLiteral(value)
      case IRNullLiteral(_)        => SIRNullLiteral
    }

    private def generateLocalName: String = "ProteaJLocalContext$$" + generateUniqueId
  }
}
