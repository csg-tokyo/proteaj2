package phenan.prj.typing

import phenan.prj._

class Unifier (compiler: JCompiler) {
  type MetaArgs = Map[String, MetaValue]

  def unify (t: JType, gt: JGenericType): Option[MetaArgs] = TypeUnifier.check(t, gt.signature, gt.env)
  def infer (t: JType, gt: JGenericType): Option[MetaArgs] = TypeInferencer.check(t, gt.signature, gt.env)

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

    def check (prm: JPrimitiveType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = {
      if (compiler.typeLoader.fromPrimitiveSignature(pts) == prm) Some(args)
      else None
    }
    def check (prm: JPrimitiveType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (prm: JPrimitiveType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (prm: JPrimitiveType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (prm: JPrimitiveType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (tvr: JTypeVariable, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(tvr, tvs, args)
    def check (tvr: JTypeVariable, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = check(tvr, cws.upperBound.getOrElse(JTypeSignature.objectTypeSig), args)

    def check (cap: JCapturedWildcardType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (cap: JCapturedWildcardType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = cap.lowerBound.flatMap(check(_, cts, args))
    def check (cap: JCapturedWildcardType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = cap.lowerBound.flatMap(check(_, ats, args))
    def check (cap: JCapturedWildcardType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = cap.lowerBound.flatMap(check(_, cws, args))
    def check (cap: JCapturedWildcardType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(cap, tvs, args)

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

  trait TypeArgumentUnifier extends MetaValueChecker[Option[MetaArgs]] {
    def check (obj: JObjectType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = cts match {
      case SimpleClassTypeSignature(clazz, as) if obj.erase.internalName == clazz =>
        val objArgs = obj.erase.signature.metaParams.flatMap(param => obj.env.get(param.name))
        if (objArgs.size == as.size) checkArgs(objArgs.zip(as), args)
        else None
      case _: SimpleClassTypeSignature => None
      case _: MemberClassTypeSignature => ???
    }

    def check (obj: JObjectType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(obj, tvs, args)

    def check (obj: JObjectType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (obj: JObjectType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (obj: JObjectType, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = None
    def check (obj: JObjectType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (obj: JObjectType, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (wc: JWildcard, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (wc: JWildcard, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(wc, tvs, args)
    def check (wc: JWildcard, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (ary: JArrayType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(ary, tvs, args)
    def check (ary: JArrayType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ExactTypeUnifier.check(ary.componentType, ats.component, args)
    def check (ary: JArrayType, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (ary: JArrayType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (ary: JArrayType, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = None
    def check (ary: JArrayType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (ary: JArrayType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (cap: JCapturedWildcardType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(cap, tvs, args)
    def check (cap: JCapturedWildcardType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (cap: JCapturedWildcardType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (cap: JCapturedWildcardType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (cap: JCapturedWildcardType, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = None
    def check (cap: JCapturedWildcardType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (cap: JCapturedWildcardType, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (pv: PureValue, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = {
      if (args.contains(pvs.name)) {
        if (pv == args(pvs.name)) Some(args)
        else None
      }
      else Some(args + (pvs.name -> pv))
    }

    def check (pv: PureValue, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (pv: PureValue, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (pv: PureValue, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (pv: PureValue, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (pv: PureValue, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = None
    def check (pv: PureValue, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (tvr: JTypeVariable, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(tvr, tvs, args)
    def check (tvr: JTypeVariable, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None

    private def checkArgs (args: List[(MetaValue, JTypeArgument)], env: MetaArgs): Option[MetaArgs] = args match {
      case (mv, arg) :: rest => ExactTypeUnifier.check(mv, arg, env) match {
        case Some(e) => checkArgs(rest, e)
        case None    => None
      }
      case Nil => Some(env)
    }

    private def typeVariableSignature (mv: MetaValue, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = {
      if (args.contains(tvs.name)) {
        if (mv == args(tvs.name)) Some(args)
        else None
      }
      else Some(args + (tvs.name -> mv))
    }
  }

  object MetaValueUnifier extends TypeArgumentUnifier {
    def check (wc: JWildcard, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = wc match {
      case JWildcard(Some(ub1), None) => wld match {
        case WildcardArgument(ub2, None) => TypeUnifier.check(ub1, ub2.getOrElse(JTypeSignature.objectTypeSig), args)
        case _ if compiler.typeLoader.objectType.contains(ub1) => Some(args)
        case _ => None
      }
      case JWildcard(None, Some(lb1)) => wld match {
        case WildcardArgument(None, Some(lb2)) => TypeInferencer.check(lb1, lb2, args)
        case _ => None
      }
      case JWildcard(None, None) => Some(args)
    }

    def check (wc: JWildcard, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = checkWildcard(wc, cts, args)

    def check (wc: JWildcard, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = checkWildcard(wc, ats, args)

    def check (wc: JWildcard, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = checkWildcard(wc, cws, args)

    private def checkWildcard (wc: JWildcard, sig: JTypeSignature, args: MetaArgs): Option[MetaArgs] = wc match {
      case JWildcard(Some(ub), None) => TypeUnifier.check(ub, sig, args)
      case JWildcard(None, Some(lb)) => TypeInferencer.check(lb, sig, args)
      case JWildcard(None, None) => Some(args)
    }
  }

  object ExactTypeUnifier extends TypeArgumentUnifier {
    def check (t: JType, ts: JTypeSignature, args: MetaArgs): Option[MetaArgs] = t match {
      case ref: JRefType => super.check(ref, ts, args)
      case prm: JPrimitiveType => check(prm, ts, args)
    }

    def check (prm: JPrimitiveType, ts: JTypeSignature, args: MetaArgs): Option[MetaArgs] = ts match {
      case pts: JPrimitiveTypeSignature if compiler.typeLoader.fromPrimitiveSignature(pts) == prm => Some(args)
      case _ => None
    }

    def check (wc: JWildcard, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = wc match {
      case JWildcard(Some(ub1), None) => wld match {
        case WildcardArgument(ub2, None) => super.check(ub1, ub2.getOrElse(JTypeSignature.objectTypeSig), args)
        case _ => None
      }
      case JWildcard(None, Some(lb1)) => wld match {
        case WildcardArgument(None, Some(lb2)) => super.check(lb1, lb2, args)
        case _ => None
      }
      case JWildcard(None, None) => wld match {
        case WildcardArgument(None, None) => Some(args)
        case WildcardArgument(Some(obj), None) if JTypeSignature.objectTypeSig == obj => Some(args)
        case _ => None
      }
    }

    def check (wc: JWildcard, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (wc: JWildcard, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (wc: JWildcard, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None
  }

  object TypeInferencer extends TypeChecker[Option[MetaArgs]] {
    // type <:< signature

    def check (obj: JObjectType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (obj: JObjectType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = unifyClass(List(obj), Set.empty, cts).flatMap { t =>
      cts match {
        case SimpleClassTypeSignature(_, as) =>
          val objArgs = t.erase.signature.metaParams.flatMap(param => t.env.get(param.name))
          if (objArgs.size == as.size) checkArgs(objArgs.zip(as), args)
          else None
        case MemberClassTypeSignature(outer, _, as) => ???
      }
    }

    def check (obj: JObjectType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (obj: JObjectType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = cws.lowerBound.flatMap(check(obj, _, args))

    def check (obj: JObjectType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(obj, tvs, args)

    def check (ary: JArrayType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (ary: JArrayType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = {
      if (CommonNames.superClassesOfArray.contains(cts.internalName)) Some(args)
      else None
    }

    def check (ary: JArrayType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = cws.lowerBound.flatMap(check(ary, _, args))

    def check (ary: JArrayType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = check(ary.componentType, ats.component, args)

    def check (ary: JArrayType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(ary, tvs, args)

    def check (prm: JPrimitiveType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = {
      if (compiler.typeLoader.fromPrimitiveSignature(pts) == prm) Some(args)
      else None
    }

    def check (prm: JPrimitiveType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (prm: JPrimitiveType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (prm: JPrimitiveType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (prm: JPrimitiveType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (tvr: JTypeVariable, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = checkBounds(tvr.bounds, cts, args)
    def check (tvr: JTypeVariable, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = checkBounds(tvr.bounds, ats, args)
    def check (tvr: JTypeVariable, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = checkBounds(tvr.bounds, cws, args)
    def check (tvr: JTypeVariable, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(tvr, tvs, args)

    def check (cap: JCapturedWildcardType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (cap: JCapturedWildcardType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = check(cap.upperBound, cts, args)
    def check (cap: JCapturedWildcardType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = check(cap.upperBound, ats, args)
    def check (cap: JCapturedWildcardType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = check(cap.upperBound, cws, args)
    def check (cap: JCapturedWildcardType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(cap, tvs, args)

    private def typeVariableSignature (rt: JRefType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = {
      if (args.contains(tvs.name)) args(tvs.name) match {
        case ref: JRefType if rt <:< ref         => Some(args)
        case JWildcard(_, Some(lb)) if rt <:< lb => Some(args)
        case _ => None
      }
      else Some(args + (tvs.name -> rt))
    }

    private def unifyClass (types: List[JObjectType], checked: Set[JObjectType], cts: JClassTypeSignature): Option[JObjectType] = types match {
      case t :: rest if t.erase.internalName == cts.internalName => Some(t)
      case t :: rest => unifyClass(rest ++ t.superTypes.filterNot(checked.contains), checked + t, cts)
      case Nil => None
    }

    private def checkBounds (bounds: List[JRefType], sig: JTypeSignature, args: MetaArgs): Option[MetaArgs] = bounds match {
      case bound :: rest => check(bound, sig, args) match {
        case Some(e) => Some(e)
        case None    => checkBounds(rest, sig, args)
      }
      case Nil => compiler.typeLoader.objectType.flatMap(check(_, sig, args))
    }

    private def checkArgs (args: List[(MetaValue, JTypeArgument)], env: MetaArgs): Option[MetaArgs] = args match {
      case (mv, arg) :: rest => MetaValueInferencer.check(mv, arg, env) match {
        case Some(e) => checkArgs(rest, e)
        case None    => None
      }
      case Nil => Some(env)
    }
  }

  object MetaValueInferencer extends MetaValueChecker[Option[MetaArgs]] {
    def check (obj: JObjectType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = cts match {
      case SimpleClassTypeSignature(clazz, as) if obj.erase.internalName == clazz =>
        val objArgs = obj.erase.signature.metaParams.flatMap(param => obj.env.get(param.name))
        if (objArgs.size == as.size) checkArgs(objArgs.zip(as), args)
        else None
      case _: SimpleClassTypeSignature => None
      case _: MemberClassTypeSignature => ???
    }

    def check (obj: JObjectType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(obj, tvs, args)

    def check (obj: JObjectType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (obj: JObjectType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (obj: JObjectType, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = checkWildcard(obj, wld, args)
    def check (obj: JObjectType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (obj: JObjectType, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (wc: JWildcard, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (wc: JWildcard, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (wc: JWildcard, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (wc: JWildcard, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (wc: JWildcard, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(wc, tvs, args)
    def check (wc: JWildcard, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (wc: JWildcard, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = wld match {
      case WildcardArgument(Some(ub1), None) => wc match {
        case JWildcard(Some(ub2), None) => TypeInferencer.check(ub2, ub1, args)
        case _ if compiler.typeLoader.objectType.contains(ub1) => Some(args)
        case _ => None
      }
      case WildcardArgument(None, Some(lb1)) => wc match {
        case JWildcard(None, Some(lb2)) => TypeUnifier.check(lb2, lb1, args)
        case _ => None
      }
      case WildcardArgument(None, None) => Some(args)
    }

    def check (ary: JArrayType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(ary, tvs, args)
    def check (ary: JArrayType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ExactTypeUnifier.check(ary.componentType, ats.component, args)
    def check (ary: JArrayType, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (ary: JArrayType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (ary: JArrayType, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = checkWildcard(ary, wld, args)
    def check (ary: JArrayType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (ary: JArrayType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (cap: JCapturedWildcardType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(cap, tvs, args)
    def check (cap: JCapturedWildcardType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (cap: JCapturedWildcardType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (cap: JCapturedWildcardType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (cap: JCapturedWildcardType, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = checkWildcard(cap, wld, args)
    def check (cap: JCapturedWildcardType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (cap: JCapturedWildcardType, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (pv: PureValue, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = {
      if (args.contains(pvs.name)) {
        if (pv == args(pvs.name)) Some(args)
        else None
      }
      else Some(args + (pvs.name -> pv))
    }

    def check (pv: PureValue, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (pv: PureValue, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (pv: PureValue, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (pv: PureValue, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (pv: PureValue, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = None
    def check (pv: PureValue, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (tvr: JTypeVariable, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = typeVariableSignature(tvr, tvs, args)
    def check (tvr: JTypeVariable, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, pvs: PureVariableSignature, args: MetaArgs): Option[MetaArgs] = None
    def check (tvr: JTypeVariable, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = checkWildcard(tvr, wld, args)
    def check (tvr: JTypeVariable, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = None

    private def checkArgs (args: List[(MetaValue, JTypeArgument)], env: MetaArgs): Option[MetaArgs] = args match {
      case (mv, arg) :: rest => ExactTypeUnifier.check(mv, arg, env) match {
        case Some(e) => checkArgs(rest, e)
        case None    => None
      }
      case Nil => Some(env)
    }

    private def typeVariableSignature (mv: MetaValue, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = {
      if (args.contains(tvs.name)) {
        if (mv == args(tvs.name)) Some(args)
        else None
      }
      else Some(args + (tvs.name -> mv))
    }

    private def checkWildcard (ref: JRefType, wld: WildcardArgument, args: MetaArgs): Option[MetaArgs] = wld match {
      case WildcardArgument(Some(ub), None) => TypeInferencer.check(ref, ub, args)
      case WildcardArgument(None, Some(lb)) => TypeUnifier.check(ref, lb, args)
      case WildcardArgument(None, None) => Some(args)
    }
  }
}



