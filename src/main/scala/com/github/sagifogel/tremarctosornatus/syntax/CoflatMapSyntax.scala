package com.github.sagifogel.tremarctosornatus.syntax

import cats.CoflatMap
import cats.data.Cokleisli

object CoflatMapSyntax {
  implicit class CoflatMapOps[F[_], A, B](val f: Cokleisli[F, A, B]) extends AnyVal {
    def =>=[C](g: Cokleisli[F, B, C])(implicit CF: CoflatMap[F]): Cokleisli[F, A, C] =
      g compose f
  }
}
