package org.qirx.cms.machinery

import scala.language.implicitConversions
import scala.language.higherKinds
import org.qirx.cms.construction.Structure
import qirx.Co

object BuildTools extends Branching {

  //  implicit def toTypeInferredProgram[F[_], A](fa:F[A]):TypeInferredProgram[F, A] = TypeInferredProgram(fa)

  trait TypeList
  trait Defaults extends TypeList
  trait +[Types <: TypeList, Type[_]] extends TypeList

  object AvailableParts {
    def apply[Types <: TypeList](
      implicit coProduct: ToCoproduct[Types]): AvailableParts[coProduct.Out] = null
  }

  trait ToCoproduct[Types <: TypeList] {
    type Out[_]
  }

  object ToCoproduct {

    implicit def fromTypeList[Types <: TypeList, Type[_]](
      implicit forTypes: ToCoproduct[Types]): ToCoproduct[Types + Type] {
      type Out[x] = Co[Type, forTypes.Out]#Product[x]
    } = null

    implicit def fromDefaults[Type[_]]: ToCoproduct[Defaults + Type] {
      type Out[x] = Co[Type, Structure]#Product[x]
    } = null
  }

  implicit def toProgram[F[_], A, O[_]](fa: F[A])(
    implicit parts: AvailableParts[O],
    lift: F ~> O): Program[O, A] = Program(fa)
}