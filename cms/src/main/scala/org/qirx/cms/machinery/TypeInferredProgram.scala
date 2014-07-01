package org.qirx.cms.machinery

import scala.language.higherKinds

case class TypeInferredProgram[F[_], A](program: Program[F, A]) {

  import Coproduct.|

  def flatMap[G[_], B](f: A => TypeInferredProgram[G, B])(
    implicit c: F | G): TypeInferredProgram[c.Out, B] =
    TypeInferredProgram(program
      .mapSuspension(c.transformLeft)
      .flatMap(f andThen (_.program.mapSuspension(c.transformRight))))

  def mapSuspension[G[_]](implicit transform: F ~> G): TypeInferredProgram[G, A] =
    TypeInferredProgram(program.mapSuspension[G])

  def map[B](f: A => B): TypeInferredProgram[F, B] =
    TypeInferredProgram(program.map(f))

  def run[G[_]: Free.Monad](transform: F ~> G): G[A] =
    program.foldMap(transform)

  // pattern match support
  def withFilter(f: Any => Boolean): TypeInferredProgram[F, A] =
    this
}

object TypeInferredProgram {

  def apply[F[_], A](fa: F[A]): TypeInferredProgram[F, A] =
    TypeInferredProgram(Free(fa))
}