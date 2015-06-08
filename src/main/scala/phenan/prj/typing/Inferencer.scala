package phenan.prj.typing

import phenan.prj._

class Inferencer (compiler: JCompiler) {
  type MetaArgs = Map[String, MetaValue]

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

    private def checkArgs (args: List[(MetaValue, JTypeArgument)], env: MetaArgs): Option[MetaArgs] = ???
  }
}
