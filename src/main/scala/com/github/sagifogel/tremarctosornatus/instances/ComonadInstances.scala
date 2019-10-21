package com.github.sagifogel.tremarctosornatus.instances

import cats.{Comonad, Functor}
import com.github.sagifogel.tremarctosornatus.FocusedImage
import com.github.sagifogel.tremarctosornatus.syntax.FocusedImageSyntax._

import scala.math.Integral.Implicits._

object ComonadInstances {
  val focusedImageFunctorInstance: Functor[FocusedImage] = new Functor[FocusedImage] {
    override def map[A, B](fa: FocusedImage[A])(f: A => B): FocusedImage[B] =
      fa.copy(vector = fa.vector.map(f))
  }

  val focusedImageCoflaMapInstance: Comonad[FocusedImage] = new Comonad[FocusedImage] {
    override def extract[A](focusedImage: FocusedImage[A]): A =
      focusedImage.vector(focusedImage.index)

    override def coflatMap[A, B](fa: FocusedImage[A])(f: FocusedImage[A] => B): FocusedImage[B] = {
      val width = fa.buffer.getWidth

      FocusedImage[B](Vector.tabulate[B](fa.vector.length)(i => {
        val (y, x) = i /% width

        f(FocusedImage(fa.vector, x, y, fa.buffer))
      }), fa.x, fa.y, fa.buffer)
    }


    override def map[A, B](fa: FocusedImage[A])(f: A => B): FocusedImage[B] =
      focusedImageFunctorInstance.map(fa)(f)
  }
}
