package org.qirx.cms.machinery

import scala.language.higherKinds
import org.qirx.cms.construction
import org.qirx.cms.construction.System

sealed trait TypeSet {
  type Out[_]
}

object TypeSet {

  type Base = TypeSet {
    type Out[x] = System[x]
  }
  type +[Types <: TypeSet, Type[_]] = TypeSet {
    type Out[x] = Co[Type, Types#Out]#Product[x]
  }

  trait ToTypeSet[Type[_]] {
    type Out <: TypeSet
  }

  object ToTypeSet {
    import Coproduct.::

    implicit def fromCoProduct[Head[_], Tail[_]](
      implicit tailSet: ToTypeSet[Tail]): ToTypeSet[(Head :: Tail)#T] {
      type Out = tailSet.Out + Head
    } = null

    implicit def fromCoProductWithSystem[Head[_]](
      implicit tailSet: ToTypeSet[Head]): ToTypeSet[(Head :: System)#T] {
      type Out = Base + Head
    } = null
  }
}
