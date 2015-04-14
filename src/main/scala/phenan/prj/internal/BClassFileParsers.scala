package phenan.prj.internal

import java.io._

import phenan.prj.exception._
import phenan.prj.state.JState

class BClassFileParsers (implicit state: JState) extends ByteParsers {

  def fromFile (file: File) = parse(file)(classFile)
  def fromFileName (file: String) = parseFile(file)(classFile)
  def fromStream (stream: InputStream) = parse(stream)(classFile)

  lazy val classFile = for {
    magic <- s4 if magic == 0xcafebabe
    minor <- u2
    major <- u2
    cPool <- constantPool
    flags <- u2
    cls   <- u2
    sup   <- u2
    ifs   <- list(u2)
    fs    <- list(field)
    ms    <- list(method)
    attrs <- list(attribute)
  } yield BClassFile(minor, major, cPool, flags, cls, sup, ifs, fs, ms, attrs)

  lazy val constantPool = for {
    max <- u2
    map <- constantMap(1, max)
  } yield BConstantPool(map)

  private def constantMap (index: Int, max: Int): ByteParser[Map[Int, BConstant]] = {
    if (index < max) for {
      c <- constant
      m <- constantMap(getNextIndex(index, c), max)
    } yield m + (index -> c)
    else ret(Map.empty)
  }

  private def getNextIndex (index: Int, c: BConstant) = c match {
    case BLongValue(_) | BDoubleValue(_) => index + 2
    case _ => index + 1
  }

  lazy val constant = u1 >>= {
    case 0x01 => for (value <- utf) yield BUtf8Value(value)
    case 0x03 => for (value <- s4)  yield BIntValue(value)
    case 0x04 => for (value <- f4)  yield BFloatValue(value)
    case 0x05 => for (value <- s8)  yield BLongValue(value)
    case 0x06 => for (value <- f8)  yield BDoubleValue(value)
    case 0x07 => for (ref <- u2)    yield BClassRef(ref)
    case 0x08 => for (ref <- u2)    yield BStringRef(ref)
    case 0x09 => for (cls <- u2; sig <- u2) yield BFieldRef(cls, sig)
    case 0x0a => for (cls <- u2; sig <- u2) yield BMethodRef(cls, sig)
    case 0x0b => for (cls <- u2; sig <- u2) yield BIMethodRef(cls, sig)
    case 0x0c => for (nam <- u2; typ <- u2) yield BNameTypeRef(nam, typ)
    case 0x0f => for (knd <- u1; ind <- u2) yield BMethodHandleInfo(knd, ind)
    case 0x10 => for (dsc <- u2)            yield BMethodTypeInfo(dsc)
    case 0x12 => for (bst <- u2; sig <- u2) yield BInvokeDynamicInfo(bst, sig)
    case n    => failure(InvalidClassFileException("wrong constant tag: " + n))
  }

  lazy val field = for {
    mod  <- u2
    name <- u2
    desc <- u2
    attr <- list(attribute)
  } yield BField(mod, name, desc, attr)

  lazy val method = for {
    mod  <- u2
    name <- u2
    desc <- u2
    attr <- list(attribute)
  } yield BMethod(mod, name, desc, attr)

  lazy val attribute = for {
    tag <- u2
    len <- s4
    dat <- bytes(len)
  } yield BAttributeInfo(tag, dat)

}
