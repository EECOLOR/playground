package org.qirx.cms.machinery

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.annotation.implicitNotFound

object Coproduct {

  def apply[Head[_], Tail[_], x](value: Either[Head[x], Tail[x]])(
    implicit ev: Coproduct.NotAtLeft[Head]) = Co[Head, Tail].Product(value)

  @implicitNotFound("There can be no coproducts on the left, import Coproduct.proof._ if this is wrong, found: ${F}")
  type NotAtLeft[F[_]] = IsNotCoproduct[F]

  trait IsCoproduct[F[_]]

  object IsCoproduct {
    implicit def coproduct[Head[_], Tail[_]]: IsCoproduct[Co[Head, Tail]#Product] = null
  }

  trait IsNotCoproduct[F[_]]

  object IsNotCoproduct {
    /*
       For types that are a coproduct we provide an ambigous IsNot
       value, this makes the real one unusable
     */
    implicit def isCoproduct[F[_]](implicit ev: IsCoproduct[F]): IsNotCoproduct[F] = null
    implicit def isNotCoproduct[F[_]]: IsNotCoproduct[F] = null
    implicit def nothingIsNotCoproduct: IsNotCoproduct[Nothing] = null
  }

  private type ::[Head[_], Tail[_]] = Co[Head, Tail]

  trait IsIdentity[F[_]]
  object IsIdentity {
    type Id[x] = x
    implicit def identity[F[_]](implicit ev: F[_] =:= Id[_]): IsIdentity[F] = null
  }

  trait IsNotIdentity[F[_]]
  object IsNotIdentity {
    implicit def identity[F[_]](implicit ev: IsIdentity[F]): IsNotIdentity[F] = null
    implicit def notIdentity[F[_]]: IsNotIdentity[F] = null
  }

  trait LowerPriorityTransformations {

    implicit def none[Elem[_]](
      implicit ev: IsNotIdentity[Elem]) =
      new (Elem ~> Elem) {
        def transform[x] = identity
      }

    implicit def atHead[Elem[_], Tail[_]](
      implicit ev: IsNotCoproduct[Elem]) =
      new (Elem ~> (Elem :: Tail)#T) {

        def transform[x] = elem =>
          new (Elem :: Tail).Product(Left(elem))
      }

    implicit def inTail[Elem[_], Head[_], Tail[_]](
      implicit ev1: IsNotCoproduct[Elem],
      ev2: IsNotCoproduct[Head],
      transformTail: Elem ~> Tail) =
      new (Elem ~> (Head :: Tail)#T) {
        def transform[x] = elem =>
          new (Head :: Tail).Product(Right(transformTail(elem)))
      }
  }

  trait Transformations extends LowerPriorityTransformations {

    implicit def isCoProduct[Head[_], Tail[_], Target[_]](
      implicit ev: IsNotCoproduct[Head],
      transformHead: Head ~> Target,
      transformTail: Tail ~> Target) =
      new ((Head :: Tail)#T ~> Target) {
        def transform[x] =
          _.value match {
            case Left(head) => transformHead(head)
            case Right(tail) => transformTail(tail)
          }
      }

    implicit def transformSource[F[_], Target[_], G[_]](fToTarget: F ~> Target)(
      implicit gToF: G ~> F): G ~> Target = gToF andThen fToTarget
  }
}