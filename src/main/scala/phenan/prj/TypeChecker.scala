package phenan.prj

trait TypeChecker[T] {
  def check (t: JType, sig: JTypeSignature): T = t match {
    case ref: JRefType       => check(ref, sig)
    case prm: JPrimitiveType => check(prm, sig)
  }
  def check (ref: JRefType, sig: JTypeSignature): T = ref match {
    case obj: JObjectType   => check(obj, sig)
    case ary: JArrayType    => check(ary, sig)
    case tvr: JTypeVariable => check(tvr, sig)
    case cap: JCapturedWildcardType => check(cap, sig)
  }
  def check (obj: JObjectType, sig: JTypeSignature): T = sig match {
    case cts: JClassTypeSignature     => check(obj, cts)
    case pts: JPrimitiveTypeSignature => check(obj, pts)
    case ats: JArrayTypeSignature     => check(obj, ats)
    case tvs: JTypeVariableSignature  => check(obj, tvs)
    case cws: JCapturedWildcardSignature => check(obj, cws)
  }
  def check (ary: JArrayType, sig: JTypeSignature): T = sig match {
    case cts: JClassTypeSignature     => check(ary, cts)
    case pts: JPrimitiveTypeSignature => check(ary, pts)
    case ats: JArrayTypeSignature     => check(ary, ats)
    case tvs: JTypeVariableSignature  => check(ary, tvs)
    case cws: JCapturedWildcardSignature => check(ary, cws)
  }
  def check (tvr: JTypeVariable, sig: JTypeSignature): T = sig match {
    case cts: JClassTypeSignature     => check(tvr, cts)
    case pts: JPrimitiveTypeSignature => check(tvr, pts)
    case ats: JArrayTypeSignature     => check(tvr, ats)
    case tvs: JTypeVariableSignature  => check(tvr, tvs)
    case cws: JCapturedWildcardSignature => check(tvr, cws)
  }
  def check (cap: JCapturedWildcardType, sig: JTypeSignature): T = sig match {
    case cts: JClassTypeSignature     => check(cap, cts)
    case pts: JPrimitiveTypeSignature => check(cap, pts)
    case ats: JArrayTypeSignature     => check(cap, ats)
    case tvs: JTypeVariableSignature  => check(cap, tvs)
    case cws: JCapturedWildcardSignature => check(cap, cws)
  }
  def check (prm: JPrimitiveType, sig: JTypeSignature): T = sig match {
    case cts: JClassTypeSignature     => check(prm, cts)
    case pts: JPrimitiveTypeSignature => check(prm, pts)
    case ats: JArrayTypeSignature     => check(prm, ats)
    case tvs: JTypeVariableSignature  => check(prm, tvs)
    case cws: JCapturedWildcardSignature => check(prm, cws)
  }

  def check (obj: JObjectType, cts: JClassTypeSignature): T
  def check (obj: JObjectType, pts: JPrimitiveTypeSignature): T
  def check (obj: JObjectType, ats: JArrayTypeSignature): T
  def check (obj: JObjectType, tvs: JTypeVariableSignature): T
  def check (obj: JObjectType, cws: JCapturedWildcardSignature): T

  def check (ary: JArrayType, cts: JClassTypeSignature): T
  def check (ary: JArrayType, pts: JPrimitiveTypeSignature): T
  def check (ary: JArrayType, ats: JArrayTypeSignature): T
  def check (ary: JArrayType, tvs: JTypeVariableSignature): T
  def check (ary: JArrayType, cws: JCapturedWildcardSignature): T

  def check (tvr: JTypeVariable, cts: JClassTypeSignature): T
  def check (tvr: JTypeVariable, pts: JPrimitiveTypeSignature): T
  def check (tvr: JTypeVariable, ats: JArrayTypeSignature): T
  def check (tvr: JTypeVariable, tvs: JTypeVariableSignature): T
  def check (tvr: JTypeVariable, cws: JCapturedWildcardSignature): T

  def check (cap: JCapturedWildcardType, cts: JClassTypeSignature): T
  def check (cap: JCapturedWildcardType, pts: JPrimitiveTypeSignature): T
  def check (cap: JCapturedWildcardType, ats: JArrayTypeSignature): T
  def check (cap: JCapturedWildcardType, tvs: JTypeVariableSignature): T
  def check (cap: JCapturedWildcardType, cws: JCapturedWildcardSignature): T

  def check (prm: JPrimitiveType, cts: JClassTypeSignature): T
  def check (prm: JPrimitiveType, pts: JPrimitiveTypeSignature): T
  def check (prm: JPrimitiveType, ats: JArrayTypeSignature): T
  def check (prm: JPrimitiveType, tvs: JTypeVariableSignature): T
  def check (prm: JPrimitiveType, cws: JCapturedWildcardSignature): T
}

trait MetaValueChecker[T] extends TypeChecker[T] {
  def check (mv: MetaValue, arg: JTypeArgument): T = mv match {
    case rt: JRefType  => check(rt, arg)
    case pv: PureValue => check(pv, arg)
    case wc: JWildcard => check(wc, arg)
  }
  def check (rt: JRefType, arg: JTypeArgument): T
  def check (pv: PureValue, arg: JTypeArgument): T
  def check (wc: JWildcard, arg: JTypeArgument): T
}
