package org.qirx.cms.machinery

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import _root_.qirx.Co

object Coproduct {

  def apply[Head[_], Tail[_], x](value: Either[Head[x], Tail[x]])(
    implicit ev: Coproduct.NotAtLeft[Head]) = Co[Head, Tail].Product(value)

  trait Is[F[_]]

  object Is {
    implicit def coproduct[Head[_], Tail[_]]: Is[Co[Head, Tail]#Product] = null
  }

  @implicitNotFound("There can be no coproducts on the left, import Coproduct.proof._ if this is wrong, found: ${F}")
  type NotAtLeft[F[_]] = IsNotCoproduct[F]

  type IsNotCoproduct[F[_]] = IsNot[F]

  trait IsNot[F[_]]

  object IsNot {
    /*
       For types that are a coproduct we provide an ambigous IsNot
       value, this makes the real one unusable
     */
    implicit def isCoproduct[F[_]](implicit ev: Is[F]): IsNot[F] = null
    implicit def isNotCoproduct[F[_]]: IsNot[F] = null
    implicit def nothingIsNotCoproduct: IsNot[Nothing] = null
  }

  type ::[Head[_], Tail[_]] = Co[Head, Tail]

  trait contains[List[_], Elem[_]]

  object contains {

    implicit def atHead[Head[_], Tail[_]]: (Head :: Tail)#T contains Head = null

    implicit def inTail[Elem[_], Head[_], Tail[_]](
      implicit ev: Tail contains Elem): (Head :: Tail)#T contains Elem = null

    implicit def isElem[Elem[_]]: Elem contains Elem = null
  }

  trait union[Left[_], Right[_]] {
    type Out[_]
  }

  trait LowestPriorityUnion {

    implicit def pair[Left[_], Right[_]](
      implicit ev: IsNotCoproduct[Left]): (Left union Right) {
      type Out[x] = (Left :: Right)#T[x]
    } = null

    implicit def headNotInRight[Head[_], Tail[_], Right[_]](
      implicit tailRightUnion: Tail union Right): ((Head :: Tail)#T union Right) {
      type Out[x] = (Head :: tailRightUnion.Out)#T[x]
    } = null
  }

  trait LowerPriorityUnion extends LowestPriorityUnion {

    implicit def headInRight[Head[_], Tail[_], Right[_]](
      implicit ev: Right contains Head,
      tailRightUnion: Tail union Right): ((Head :: Tail)#T union Right) {
      type Out[x] = tailRightUnion.Out[x]
    } = null
  }

  object union extends LowerPriorityUnion {

    // this crashes the compiler
    //type Aux[Left[_], Right[_], O[_]] = (Left union Right) { type Out[x] = O[x] }
    trait Aux[Left[_], Right[_], O[_]]
    object Aux {
      implicit def proxy[Left[_], Right[_]](
        implicit u: Left union Right): Aux[Left, Right, u.Out] = null
    }

    implicit def leftContainsRight[Head[_], Tail[_], Right[_]](
      implicit ev: (Head :: Tail)#T contains Right): ((Head :: Tail)#T union Right) {
      type Out[x] = (Head :: Tail)#T[x]
    } = null

    implicit def rightContainsLeft[Left[_], Head[_], Tail[_]](
      implicit ev: (Head :: Tail)#T contains Left): (Left union (Head :: Tail)#T) {
      type Out[x] = (Head :: Tail)#T[x]
    } = null

    implicit def same[Elem[_]]: (Elem union Elem) {
      type Out[x] = Elem[x]
    } = null
  }

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

  trait LowerPriorityTransformations_3 {
    
    implicit def none[Elem[_]](
      implicit ev: IsNotIdentity[Elem]) =
      new (Elem ~> Elem) {
        def apply[x](elem: Elem[x]) = elem
      }
  }
  
  trait LowerPriorityTransformations_2 extends LowerPriorityTransformations_3 {
    implicit def atHead[Elem[_], Tail[_]](
      implicit ev: IsNotCoproduct[Elem]) =
      new (Elem ~> (Elem :: Tail)#T) {

        def apply[x](elem: Elem[x]) =
          new (Elem :: Tail).Product(Left(elem))
      }
  }
  
  trait LowerPriorityTransformations_1 extends LowerPriorityTransformations_2 {

    implicit def inTail[Elem[_], Head[_], Tail[_]](
      implicit ev1: IsNotCoproduct[Elem],
      ev2: IsNotCoproduct[Head],
      transformTail: Elem ~> Tail) =
      new (Elem ~> (Head :: Tail)#T) {
        def apply[x](elem: Elem[x]) =
          new (Head :: Tail).Product(Right(transformTail(elem)))
      }

  }

  object Transformations extends LowerPriorityTransformations_1 {

    implicit def elemIsCoProduct[Elem[_], Tail[_], Target[_]](
      implicit ev: IsNotCoproduct[Elem],
      transformHead: Elem ~> Target,
      transformTail: Tail ~> Target) =
      new ((Elem :: Tail)#T ~> Target) {
        def apply[x](elem: (Elem :: Tail)#T[x]) =
          elem.value match {
            case Left(head) => transformHead(head)
            case Right(tail) => transformTail(tail)
          }
      }

    implicit def transformSource[F[_], Target[_], G[_]](fToTarget: F ~> Target)(
      implicit transform: G ~> F) =
      new (G ~> Target) {
        def apply[x](g: G[x]) = fToTarget(transform(g))
      }

    implicit def transformTarget[F[_], Source[_], G[_]](
      implicit transform: F ~> G) =
      (sourceToF: Source ~> F) =>
        new (Source ~> G) {
          def apply[x](g: Source[x]) = transform(sourceToF(g))
        }
  }

  implicit class TranformationEnhancement[Tail[_], Target[_]](fg: Tail ~> Target) {
    def or[T[_], Head[_]](hg: Head ~> T)(implicit ev: Head ~> T => Head ~> Target) =
      new ((Head :: Tail)#T ~> Target) {
        def apply[x](elem: (Head :: Tail)#T[x]) =
          elem.value match {
            case Left(head) => ev(hg) apply head
            case Right(tail) => fg(tail)
          }
      }
  }

  type |[Left[_], Right[_]] = Combined[Left, Right]

  trait Combined[Left[_], Right[_]] {
    type Out[_]
    def transformLeft: Left ~> Out
    def transformRight: Right ~> Out
  }

  trait LowerPriorityCombined {
    implicit def combine[Left[_], Right[_], O[_]](
      implicit u: union.Aux[Left, Right, O],
      l: Left ~> O,
      r: Right ~> O) =
      new (Left | Right) {
        type Out[x] = O[x]
        val transformLeft = l
        val transformRight = r
      }
  }

  object Combined extends LowerPriorityCombined {
    type Aux[Left[_], Right[_], O[_]] = Combined[Left, Right] { type Out[x] = O[x] }

    implicit def single[Elem[_]](
      implicit isNot: IsNotIdentity[Elem]) =
      new (Elem | Elem) {
        type Out[x] = Elem[x]

        val transformLeft = Transformations.none[Elem]
        val transformRight = Transformations.none[Elem]
      }
  }
}