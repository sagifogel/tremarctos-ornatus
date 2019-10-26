package com.github.sagifogel.tremarctosornatus.syntax

import cats.CoflatMap
import cats.data.Cokleisli

object CoflatMapSyntax {
  implicit class CoflatMapOps[F[_], A, B](val f: F[A] => B) extends AnyVal {
    def =>=[C](g: F[B] => C)(implicit CF: CoflatMap[F]): F[A] => C = {
      (Cokleisli[F, B, C](g) compose Cokleisli[F, A, B](f)).run
    }
  }
}
