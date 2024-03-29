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

  implicit val focusedImageComonadInstance: Comonad[FocusedImage] = new Comonad[FocusedImage] {
    override def extract[A](fa: FocusedImage[A]): A =
      fa.pixels(fa.index)

    override def coflatMap[A, B](fa: FocusedImage[A])(f: FocusedImage[A] => B): FocusedImage[B] = {
      val width = fa.width
      val height = fa.height
      val indexedValues = Vector.range(0, fa.pixels.length).map(i => {
        val (y, x) = i /% width
        val index = x * height + y
        val value = f(FocusedImage(fa.pixels, x, y, width, height, fa.buffer))

        (index, value)
      })

      val pixels = indexedValues.sortBy(_._1).map(_._2)

      FocusedImage(pixels, fa.x, fa.y, width, height, fa.buffer)
    }

    override def map[A, B](fa: FocusedImage[A])(f: A => B): FocusedImage[B] =
      focusedImageFunctorInstance.map(fa)(f)
  }
}

