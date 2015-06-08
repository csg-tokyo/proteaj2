package phenan.prj.typing

import phenan.prj._

class Inferencer (compiler: JCompiler) {
  type MetaArgs = Map[String, MetaValue]

  object TypeInferencer extends TypeChecker[Option[MetaArgs]] {
    // type <:< signature

    def check (obj: JObjectType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (obj: JObjectType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (obj: JObjectType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (obj: JObjectType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (obj: JObjectType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (ary: JArrayType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (ary: JArrayType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = None

    def check (ary: JArrayType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (ary: JArrayType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (ary: JArrayType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

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

    def check (tvr: JTypeVariable, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (tvr: JTypeVariable, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (cap: JCapturedWildcardType, cts: JClassTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (cap: JCapturedWildcardType, pts: JPrimitiveTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (cap: JCapturedWildcardType, ats: JArrayTypeSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (cap: JCapturedWildcardType, tvs: JTypeVariableSignature, args: MetaArgs): Option[MetaArgs] = ???

    def check (cap: JCapturedWildcardType, cws: JCapturedWildcardSignature, args: MetaArgs): Option[MetaArgs] = ???

  }
}
