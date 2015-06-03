package phenan.prj.typing

import phenan.prj._

class Unifier (compiler: JCompiler) {
  type MetaArgs = Map[String, MetaValue]

  object TypeUnifier extends TypeChecker[Option[MetaArgs]] {

    // type >:> signature

    def check (obj: JObjectType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = unifyClass(obj, List(cts), Set.empty).flatMap {
      case SimpleClassTypeSignature(_, as) =>
        val objArgs = obj.erase.signature.metaParams.flatMap(param => obj.env.get(param.name))
        if (objArgs.size == as.size) checkArgs(objArgs.zip(as), args)
        else None
      case MemberClassTypeSignature(outer, _, as) => ???
    }

    def check (obj: JObjectType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = {
      if (compiler.typeLoader.superTypesOfArray.contains(obj)) Some(args)
      else None
    }

    def check (obj: JObjectType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = check(obj, cws.upperBound.getOrElse(JTypeSignature.objectTypeSig), args)

    def check (obj: JObjectType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(obj, tvs, args)

    def check (obj: JObjectType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (ary: JArrayType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (ary: JArrayType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (ary: JArrayType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = check(ary, cws.upperBound.getOrElse(JTypeSignature.objectTypeSig), args)

    def check (ary: JArrayType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = check(ary.componentType, ats.component, args)

    def check (ary: JArrayType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(ary, tvs, args)

    def check (prm: JPrimitiveType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (prm: JPrimitiveType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = {
      if (compiler.typeLoader.fromPrimitiveSignature(pts) == prm) Some(args)
      else None
    }

    def check (prm: JPrimitiveType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (prm: JPrimitiveType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (prm: JPrimitiveType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (tvr: JTypeVariable, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (tvr: JTypeVariable, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (tvr: JTypeVariable, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (tvr: JTypeVariable, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(tvr, tvs, args)

    def check (tvr: JTypeVariable, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = check(tvr, cws.upperBound.getOrElse(JTypeSignature.objectTypeSig), args)

    def check (cap: JCapturedWildcardType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = cap.lowerBound.flatMap(check(_, cts, args))

    def check (cap: JCapturedWildcardType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = cap.lowerBound.flatMap(check(_, pts, args))

    def check (cap: JCapturedWildcardType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = cap.lowerBound.flatMap(check(_, ats, args))

    def check (cap: JCapturedWildcardType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = cap.lowerBound.flatMap(check(_, tvs, args))

    def check (cap: JCapturedWildcardType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = cap.lowerBound.flatMap(check(_, cws, args))


    private def typeVariableSignature (rt: JRefType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = {
      if (args.contains(tvs.name)) args(tvs.name) match {
        case ref: JRefType if rt >:> ref             => Some(args)
        case JWildcard(ub, _) if ub.forall(rt >:> _) => Some(args)
        case _ => None
      }
      else Some(args + (tvs.name -> rt))
    }

    private def checkArgs (args: List[(MetaValue, JTypeArgument)], env: MetaArgs): Option[MetaArgs] = args match {
      case (mv, arg) :: rest => MetaValueUnifier.check(mv, arg, env) match {
        case Some(e) => checkArgs(rest, e)
        case None    => None
      }
      case Nil => Some(env)
    }

    private def unifyClass (target: JObjectType, signatures: List[JClassTypeSignature], checked: Set[JClassTypeSignature]): Option[JClassTypeSignature] = signatures match {
      case sig :: rest if sig.internalName == target.erase.internalName => Some(sig)
      case sig :: rest => unifyClass(target, rest ++ directSuperTypes(sig).filterNot(checked.contains), checked + sig)
      case Nil => None
    }

    private def directSuperTypes (signature: JClassTypeSignature): List[JClassTypeSignature] = signature match {
      case JTypeSignature.objectTypeSig => Nil
      case SimpleClassTypeSignature(clazz, args) => compiler.classLoader.loadClass_PE(clazz) match {
        case Some(cls) if cls.signature.metaParams.size == args.size =>
          (cls.signature.superClass :: cls.signature.interfaces).map(assign(_, cls.signature.metaParams.map(_.name).zip(args).toMap))
        case None => Nil
      }
      case MemberClassTypeSignature(outer, clazz, args) => ???
    }

    private def assign (sig: JTypeArgument, map: Map[String, JTypeArgument]): JTypeArgument = sig match {
      case ts : JTypeSignature         => assign(ts, map)
      case PureVariableSignature(name) => compiler.state.someOrError(map.get(name), "invalid meta variable : " + name, sig)
      case WildcardArgument(ub, lb)    => WildcardArgument(ub.map(assign(_, map)), lb.map(assign(_, map)))
    }

    private def assign (sig: JTypeSignature, map: Map[String, JTypeArgument]): JTypeSignature = sig match {
      case cts: JClassTypeSignature           => assign(cts, map)
      case prm: JPrimitiveTypeSignature       => prm
      case JArrayTypeSignature(c)             => JArrayTypeSignature(assign(c, map))
      case JTypeVariableSignature(name)       => map.get(name) match {
        case Some(signature: JTypeSignature)       => signature
        case Some(WildcardArgument(ub, lb))        => JCapturedWildcardSignature(ub.map(assign(_, map)), lb.map(assign(_, map)))
        case Some(_: PureVariableSignature) | None => compiler.state.errorAndReturn("invalid type variable : " + name, sig)
      }
      case JCapturedWildcardSignature(ub, lb) => JCapturedWildcardSignature(ub.map(assign(_, map)), lb.map(assign(_, map)))
    }

    private def assign (sig: JClassTypeSignature, map: Map[String, JTypeArgument]): JClassTypeSignature = sig match {
      case SimpleClassTypeSignature(clazz, args)        => SimpleClassTypeSignature(clazz, args.map(assign(_, map)))
      case MemberClassTypeSignature(outer, clazz, args) => MemberClassTypeSignature(assign(outer, map), clazz, args.map(assign(_, map)))
    }
  }

  object MetaValueUnifier extends MetaValueChecker[Option[MetaArgs]] {
    def check(obj: JObjectType, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cpv: ConcretePureValue, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = ???

    def check(cpv: ConcretePureValue, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cpv: ConcretePureValue, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cpv: ConcretePureValue, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cpv: ConcretePureValue, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cpv: ConcretePureValue, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(wc: JWildcard, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(wc: JWildcard, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = ???

    def check(wc: JWildcard, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(wc: JWildcard, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(wc: JWildcard, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(wc: JWildcard, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(wc: JWildcard, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cpv: ConcretePureValue, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(ary: JArrayType, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(obj: JObjectType, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = ???

    def check(ary: JArrayType, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = ???

    def check(tvr: JTypeVariable, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(tvr: JTypeVariable, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = ???

    def check(cap: JCapturedWildcardType, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cap: JCapturedWildcardType, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = ???

    def check(pvr: PureVariableRef, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(pvr: PureVariableRef, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = ???

    def check(pvr: PureVariableRef, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(pvr: PureVariableRef, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(pvr: PureVariableRef, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(pvr: PureVariableRef, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(pvr: PureVariableRef, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(obj: JObjectType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(obj: JObjectType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(ary: JArrayType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cap: JCapturedWildcardType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cap: JCapturedWildcardType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(prm: JPrimitiveType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(prm: JPrimitiveType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(prm: JPrimitiveType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(prm: JPrimitiveType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(prm: JPrimitiveType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(obj: JObjectType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(obj: JObjectType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(obj: JObjectType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cap: JCapturedWildcardType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cap: JCapturedWildcardType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(cap: JCapturedWildcardType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(tvr: JTypeVariable, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(tvr: JTypeVariable, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(tvr: JTypeVariable, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(tvr: JTypeVariable, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(tvr: JTypeVariable, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(ary: JArrayType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(ary: JArrayType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(ary: JArrayType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check(ary: JArrayType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = ???
  }
}



