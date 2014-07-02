package org.qirx.cms.machinery

import scala.language.higherKinds
import org.qirx.cms.construction
import org.qirx.cms.construction.System

sealed trait TypeSet

object TypeSet {

  trait Base extends TypeSet
  trait +[Types <: TypeSet, Type[_]] extends TypeSet

  trait ToType[Types <: TypeSet] {
    type Out[_]
  }

  object ToType {
    type Aux[Types <: TypeSet, O[_]] = ToType[Types] { type Out[x] = O[x] }

    implicit def fromTypeSet[Types <: TypeSet, Type[_]](
      implicit forTypes: ToType[Types]): ToType[Types + Type] {
      type Out[x] = Co[Type, forTypes.Out]#Product[x]
    } = null

    implicit def fromBaseAndType[Type[_]]: ToType[Base + Type] {
      type Out[x] = Co[Type, System]#Product[x]
    } = null

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
