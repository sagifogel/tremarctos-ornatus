package com.github.sagifogel.tremarctosornatus.instances

import cats.{Comonad, Functor}
import com.github.sagifogel.tremarctosornatus.data.FocusedImage
import com.github.sagifogel.tremarctosornatus.syntax.FocusedImageSyntax._

import scala.math.Integral.Implicits._

object ComonadInstances {
  def focusedImageFunctorInstance: Functor[FocusedImage] = new Functor[FocusedImage] {
    override def map[A, B](fa: FocusedImage[A])(f: A => B): FocusedImage[B] =
      fa.copy(pixels = fa.pixels.map(f))
  }

  implicit val focusedImageCoflaMapInstance: Comonad[FocusedImage] = new Comonad[FocusedImage] {
    override def extract[A](fa: FocusedImage[A]): A =
      fa.pixels(fa.index)

    override def coflatMap[A, B](fa: FocusedImage[A])(f: FocusedImage[A] => B): FocusedImage[B] = {
      FocusedImage(Vector.tabulate(fa.pixels.length)(i => {
        val (y, x) = i /% fa.buffer.getWidth

        f(FocusedImage(fa.pixels, x, y, fa.width, fa.height, fa.buffer))
      }), fa.x, fa.y, fa.width, fa.height, fa.buffer)
    }

    override def map[A, B](fa: FocusedImage[A])(f: A => B): FocusedImage[B] =
      focusedImageFunctorInstance.map(fa)(f)
  }
}
