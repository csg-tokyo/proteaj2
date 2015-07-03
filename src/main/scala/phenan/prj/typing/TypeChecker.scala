package phenan.prj.typing

import phenan.prj._

trait TypeChecker[T] {
  type MetaArgs = Map[String, MetaArgument]

  def check (t: JType, sig: JTypeSignature, args: MetaArgs): T = t match {
    case ref: JRefType       => check(ref, sig, args)
    case prm: JPrimitiveType => check(prm, sig, args)
  }
  def check (ref: JRefType, sig: JTypeSignature, args: MetaArgs): T = ref match {
    case obj: JObjectType   => check(obj, sig, args)
    case ary: JArrayType    => check(ary, sig, args)
    case tvr: JTypeVariable => check(tvr, sig, args)
    case cap: JCapturedWildcardType => check(cap, sig, args)
    case unb: JUnboundTypeVariable  => check(unb, sig, args)
  }
  def check (obj: JObjectType, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(obj, cts, args)
    case pts: JPrimitiveTypeSignature => check(obj, pts, args)
    case ats: JArrayTypeSignature     => check(obj, ats, args)
    case tvs: JTypeVariableSignature  => check(obj, tvs, args)
    case cws: JCapturedWildcardSignature => check(obj, cws, args)
  }
  def check (ary: JArrayType, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(ary, cts, args)
    case pts: JPrimitiveTypeSignature => check(ary, pts, args)
    case ats: JArrayTypeSignature     => check(ary, ats, args)
    case tvs: JTypeVariableSignature  => check(ary, tvs, args)
    case cws: JCapturedWildcardSignature => check(ary, cws, args)
  }
  def check (tvr: JTypeVariable, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(tvr, cts, args)
    case pts: JPrimitiveTypeSignature => check(tvr, pts, args)
    case ats: JArrayTypeSignature     => check(tvr, ats, args)
    case tvs: JTypeVariableSignature  => check(tvr, tvs, args)
    case cws: JCapturedWildcardSignature => check(tvr, cws, args)
  }
  def check (cap: JCapturedWildcardType, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(cap, cts, args)
    case pts: JPrimitiveTypeSignature => check(cap, pts, args)
    case ats: JArrayTypeSignature     => check(cap, ats, args)
    case tvs: JTypeVariableSignature  => check(cap, tvs, args)
    case cws: JCapturedWildcardSignature => check(cap, cws, args)
  }
  def check (unb: JUnboundTypeVariable, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(unb, cts, args)
    case pts: JPrimitiveTypeSignature => check(unb, pts, args)
    case ats: JArrayTypeSignature     => check(unb, ats, args)
    case tvs: JTypeVariableSignature  => check(unb, tvs, args)
    case cws: JCapturedWildcardSignature => check(unb, cws, args)
  }
  def check (prm: JPrimitiveType, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(prm, cts, args)
    case pts: JPrimitiveTypeSignature => check(prm, pts, args)
    case ats: JArrayTypeSignature     => check(prm, ats, args)
    case tvs: JTypeVariableSignature  => check(prm, tvs, args)
    case cws: JCapturedWildcardSignature => check(prm, cws, args)
  }

  def check (obj: JObjectType, cts: JClassTypeSignature, args: MetaArgs): T
  def check (obj: JObjectType, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (obj: JObjectType, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (obj: JObjectType, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (obj: JObjectType, cws: JCapturedWildcardSignature, args: MetaArgs): T

  def check (ary: JArrayType, cts: JClassTypeSignature, args: MetaArgs): T
  def check (ary: JArrayType, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (ary: JArrayType, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (ary: JArrayType, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (ary: JArrayType, cws: JCapturedWildcardSignature, args: MetaArgs): T

  def check (tvr: JTypeVariable, cts: JClassTypeSignature, args: MetaArgs): T
  def check (tvr: JTypeVariable, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (tvr: JTypeVariable, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (tvr: JTypeVariable, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (tvr: JTypeVariable, cws: JCapturedWildcardSignature, args: MetaArgs): T

  def check (cap: JCapturedWildcardType, cts: JClassTypeSignature, args: MetaArgs): T
  def check (cap: JCapturedWildcardType, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (cap: JCapturedWildcardType, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (cap: JCapturedWildcardType, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (cap: JCapturedWildcardType, cws: JCapturedWildcardSignature, args: MetaArgs): T

  def check (unb: JUnboundTypeVariable, cts: JClassTypeSignature, args: MetaArgs): T
  def check (unb: JUnboundTypeVariable, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (unb: JUnboundTypeVariable, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (unb: JUnboundTypeVariable, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (unb: JUnboundTypeVariable, cws: JCapturedWildcardSignature, args: MetaArgs): T

  def check (prm: JPrimitiveType, cts: JClassTypeSignature, args: MetaArgs): T
  def check (prm: JPrimitiveType, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (prm: JPrimitiveType, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (prm: JPrimitiveType, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (prm: JPrimitiveType, cws: JCapturedWildcardSignature, args: MetaArgs): T
}

trait MetaArgumentChecker[T] {
  type MetaArgs = Map[String, MetaArgument]

  def check (mv: MetaArgument, arg: JTypeArgument, args: MetaArgs): T = mv match {
    case ref: JRefType => check(ref, arg, args)
    case pv: MetaValue => check(pv, arg, args)
    case wc: JWildcard => check(wc, arg, args)
  }
  def check (ref: JRefType, arg: JTypeArgument, args: MetaArgs): T = arg match {
    case sig: JTypeSignature        => check(ref, sig, args)
    case pvs: MetaVariableSignature => check(ref, pvs, args)
    case wld: WildcardArgument      => check(ref, wld, args)
  }
  def check (pv: MetaValue, arg: JTypeArgument, args: MetaArgs): T = arg match {
    case sig: JTypeSignature        => check(pv, sig, args)
    case pvs: MetaVariableSignature => check(pv, pvs, args)
    case wld: WildcardArgument      => check(pv, wld, args)
  }
  def check (wc: JWildcard, arg: JTypeArgument, args: MetaArgs): T = arg match {
    case sig: JTypeSignature        => check(wc, sig, args)
    case pvs: MetaVariableSignature => check(wc, pvs, args)
    case wld: WildcardArgument      => check(wc, wld, args)
  }
  def check (ref: JRefType, sig: JTypeSignature, args: MetaArgs): T = ref match {
    case obj: JObjectType   => check(obj, sig, args)
    case ary: JArrayType    => check(ary, sig, args)
    case tvr: JTypeVariable => check(tvr, sig, args)
    case cap: JCapturedWildcardType => check(cap, sig, args)
    case unb: JUnboundTypeVariable  => check(unb, sig, args)
  }
  def check (obj: JObjectType, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(obj, cts, args)
    case pts: JPrimitiveTypeSignature => check(obj, pts, args)
    case ats: JArrayTypeSignature     => check(obj, ats, args)
    case tvs: JTypeVariableSignature  => check(obj, tvs, args)
    case cws: JCapturedWildcardSignature => check(obj, cws, args)
  }
  def check (ary: JArrayType, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(ary, cts, args)
    case pts: JPrimitiveTypeSignature => check(ary, pts, args)
    case ats: JArrayTypeSignature     => check(ary, ats, args)
    case tvs: JTypeVariableSignature  => check(ary, tvs, args)
    case cws: JCapturedWildcardSignature => check(ary, cws, args)
  }
  def check (tvr: JTypeVariable, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(tvr, cts, args)
    case pts: JPrimitiveTypeSignature => check(tvr, pts, args)
    case ats: JArrayTypeSignature     => check(tvr, ats, args)
    case tvs: JTypeVariableSignature  => check(tvr, tvs, args)
    case cws: JCapturedWildcardSignature => check(tvr, cws, args)
  }
  def check (cap: JCapturedWildcardType, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(cap, cts, args)
    case pts: JPrimitiveTypeSignature => check(cap, pts, args)
    case ats: JArrayTypeSignature     => check(cap, ats, args)
    case tvs: JTypeVariableSignature  => check(cap, tvs, args)
    case cws: JCapturedWildcardSignature => check(cap, cws, args)
  }
  def check (unb: JUnboundTypeVariable, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(unb, cts, args)
    case pts: JPrimitiveTypeSignature => check(unb, pts, args)
    case ats: JArrayTypeSignature     => check(unb, ats, args)
    case tvs: JTypeVariableSignature  => check(unb, tvs, args)
    case cws: JCapturedWildcardSignature => check(unb, cws, args)
  }
  def check (ref: JRefType, pvs: MetaVariableSignature, args: MetaArgs): T = ref match {
    case obj: JObjectType   => check(obj, pvs, args)
    case ary: JArrayType    => check(ary, pvs, args)
    case tvr: JTypeVariable => check(tvr, pvs, args)
    case cap: JCapturedWildcardType => check(cap, pvs, args)
    case unb: JUnboundTypeVariable  => check(unb, pvs, args)
  }
  def check (ref: JRefType, wld: WildcardArgument, args: MetaArgs): T = ref match {
    case obj: JObjectType   => check(obj, wld, args)
    case ary: JArrayType    => check(ary, wld, args)
    case tvr: JTypeVariable => check(tvr, wld, args)
    case cap: JCapturedWildcardType => check(cap, wld, args)
    case unb: JUnboundTypeVariable  => check(unb, wld, args)
  }
  def check (pv: MetaValue, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(pv, cts, args)
    case pts: JPrimitiveTypeSignature => check(pv, pts, args)
    case ats: JArrayTypeSignature     => check(pv, ats, args)
    case tvs: JTypeVariableSignature  => check(pv, tvs, args)
    case cws: JCapturedWildcardSignature => check(pv, cws, args)
  }
  def check (wc: JWildcard, sig: JTypeSignature, args: MetaArgs): T = sig match {
    case cts: JClassTypeSignature     => check(wc, cts, args)
    case pts: JPrimitiveTypeSignature => check(wc, pts, args)
    case ats: JArrayTypeSignature     => check(wc, ats, args)
    case tvs: JTypeVariableSignature  => check(wc, tvs, args)
    case cws: JCapturedWildcardSignature => check(wc, cws, args)
  }
  
  def check (obj: JObjectType, pvs: MetaVariableSignature, args: MetaArgs): T
  def check (obj: JObjectType, wld: WildcardArgument, args: MetaArgs): T
  def check (obj: JObjectType, cts: JClassTypeSignature, args: MetaArgs): T
  def check (obj: JObjectType, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (obj: JObjectType, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (obj: JObjectType, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (obj: JObjectType, cws: JCapturedWildcardSignature, args: MetaArgs): T

  def check (ary: JArrayType, pvs: MetaVariableSignature, args: MetaArgs): T
  def check (ary: JArrayType, wld: WildcardArgument, args: MetaArgs): T
  def check (ary: JArrayType, cts: JClassTypeSignature, args: MetaArgs): T
  def check (ary: JArrayType, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (ary: JArrayType, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (ary: JArrayType, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (ary: JArrayType, cws: JCapturedWildcardSignature, args: MetaArgs): T

  def check (tvr: JTypeVariable, pvs: MetaVariableSignature, args: MetaArgs): T
  def check (tvr: JTypeVariable, wld: WildcardArgument, args: MetaArgs): T
  def check (tvr: JTypeVariable, cts: JClassTypeSignature, args: MetaArgs): T
  def check (tvr: JTypeVariable, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (tvr: JTypeVariable, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (tvr: JTypeVariable, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (tvr: JTypeVariable, cws: JCapturedWildcardSignature, args: MetaArgs): T

  def check (cap: JCapturedWildcardType, pvs: MetaVariableSignature, args: MetaArgs): T
  def check (cap: JCapturedWildcardType, wld: WildcardArgument, args: MetaArgs): T
  def check (cap: JCapturedWildcardType, cts: JClassTypeSignature, args: MetaArgs): T
  def check (cap: JCapturedWildcardType, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (cap: JCapturedWildcardType, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (cap: JCapturedWildcardType, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (cap: JCapturedWildcardType, cws: JCapturedWildcardSignature, args: MetaArgs): T

  def check (unb: JUnboundTypeVariable, pvs: MetaVariableSignature, args: MetaArgs): T
  def check (unb: JUnboundTypeVariable, wld: WildcardArgument, args: MetaArgs): T
  def check (unb: JUnboundTypeVariable, cts: JClassTypeSignature, args: MetaArgs): T
  def check (unb: JUnboundTypeVariable, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (unb: JUnboundTypeVariable, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (unb: JUnboundTypeVariable, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (unb: JUnboundTypeVariable, cws: JCapturedWildcardSignature, args: MetaArgs): T

  def check (pv: MetaValue, pvs: MetaVariableSignature, args: MetaArgs): T
  def check (pv: MetaValue, wld: WildcardArgument, args: MetaArgs): T
  def check (pv: MetaValue, cts: JClassTypeSignature, args: MetaArgs): T
  def check (pv: MetaValue, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (pv: MetaValue, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (pv: MetaValue, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (pv: MetaValue, cws: JCapturedWildcardSignature, args: MetaArgs): T

  def check (wc: JWildcard, pvs: MetaVariableSignature, args: MetaArgs): T
  def check (wc: JWildcard, wld: WildcardArgument, args: MetaArgs): T
  def check (wc: JWildcard, cts: JClassTypeSignature, args: MetaArgs): T
  def check (wc: JWildcard, pts: JPrimitiveTypeSignature, args: MetaArgs): T
  def check (wc: JWildcard, ats: JArrayTypeSignature, args: MetaArgs): T
  def check (wc: JWildcard, tvs: JTypeVariableSignature, args: MetaArgs): T
  def check (wc: JWildcard, cws: JCapturedWildcardSignature, args: MetaArgs): T
}
