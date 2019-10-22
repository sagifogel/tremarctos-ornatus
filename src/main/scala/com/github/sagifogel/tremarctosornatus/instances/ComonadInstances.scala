package com.github.sagifogel.tremarctosornatus.instances

import cats.{Comonad, Functor}
import com.github.sagifogel.tremarctosornatus.FocusedImage
import com.github.sagifogel.tremarctosornatus.syntax.FocusedImageSyntax._

import scala.math.Integral.Implicits._

object ComonadInstances {
  val focusedImageFunctorInstance: Functor[FocusedImage] = new Functor[FocusedImage] {
    override def map[A, B](fa: FocusedImage[A])(f: A => B): FocusedImage[B] =
      fa.copy(pixels = fa.pixels.map(f))
  }

  implicit val focusedImageCoflaMapInstance: Comonad[FocusedImage] = new Comonad[FocusedImage] {
    override def extract[A](focusedImage: FocusedImage[A]): A =
      focusedImage.pixels(focusedImage.index)

    override def coflatMap[A, B](fa: FocusedImage[A])(f: FocusedImage[A] => B): FocusedImage[B] = {
      val width = fa.buffer.getWidth

      FocusedImage[B](Vector.tabulate(fa.pixels.length)(i => {
        val (y, x) = i /% width

        f(FocusedImage(fa.pixels, x, y, fa.buffer))
      }), fa.x, fa.y, fa.buffer)
    }

    override def map[A, B](fa: FocusedImage[A])(f: A => B): FocusedImage[B] =
      focusedImageFunctorInstance.map(fa)(f)
  }
}
