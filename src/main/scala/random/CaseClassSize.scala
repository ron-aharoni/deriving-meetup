package random

import scala.deriving._
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValue}

inline def caseClassSize[A](using m: Mirror.Of[A]): Int =
  sizeT[m.MirroredElemTypes]

inline def sizeT[A <: Tuple]: Int =
   inline erasedValue[A] match
     case _: EmptyTuple => 0
     case _: (t *: ts) => 1 + sizeT[ts]