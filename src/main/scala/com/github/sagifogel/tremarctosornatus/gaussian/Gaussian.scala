package com.github.sagifogel.tremarctosornatus.gaussian

import java.awt.image.BufferedImage

import cats.Comonad
import cats.implicits._
import com.github.sagifogel.tremarctosornatus.config.AppSettings
import com.github.sagifogel.tremarctosornatus.data.FocusedImage
import com.github.sagifogel.tremarctosornatus.gaussian.Gaussian.Live
import com.github.sagifogel.tremarctosornatus.syntax.BufferedImageSyntax._
import zio.{Task, UIO, ZIO}

trait Gaussian {
  val gaussian: Gaussian.Service
}

object Gaussian {
  trait Service {
    def convolve(config: AppSettings, buffer: BufferedImage)
                (implicit WA: Comonad[FocusedImage]): Task[BufferedImage]
  }

  trait Live extends Gaussian {
    override val gaussian: Service = new Service {
      override def convolve(config: AppSettings, buffer: BufferedImage)
                           (implicit WA: Comonad[FocusedImage]): Task[BufferedImage] =
        for {
          kernel <- createKernel(config.convolution.radius)
          focusedImage = buffer.toFocusedImage
          convolutedImage <- ZIO.effect(focusedImage.coflatMap(process(_, kernel)))
          pixels = convolutedImage.pixels.toArray
        } yield convolutedImage.buffer.fromArray(pixels, 0, 0)
    }
  }

  private def createKernel(radius: Float): UIO[Array[Float]] = UIO {
    val ceiledRadius = radius.ceil.toInt
    val sigma = radius / 3
    val sigma22 = 2 * sigma * sigma
    val sigmaPi2 = 2f * Math.PI.toFloat * sigma
    val sqrtSigmaPi2 = Math.sqrt(sigmaPi2).toFloat
    val radius2 = radius * radius
    val kernelValues = for {
      row <- -ceiledRadius to ceiledRadius
      distance = (row * row).toFloat
      value = if (distance > radius2) 0f
          else Math.exp(-distance / sigma22).toFloat / sqrtSigmaPi2
    } yield value.toFloat

    val total = kernelValues.sum

    kernelValues.foldLeft(Vector.empty[Float])(_ :+ _ / total).toArray
  }

  private def clamp(color: Float): Int = {
    val value = color.toInt
    if (value < 0) 0
    else if (value > 255) 255
    else value
  }

  def process(cell: FocusedImage[Int], kernel: Array[Float]): Int = {
    val rows = kernel.length
    val cols = kernel.length
    val rows2 = rows / 2
    val cols2 = cols / 2
    val height = cell.buffer.getHeight
    val width = cell.buffer.getWidth
    val imagePixels = cell.pixels
    val argbValues = for {
      row <- -rows2 to rows2
      col <- -cols2 to cols2
      iy = cell.y + row
      if 0 <= iy && iy < height
      ioffset = iy * width
      moffset = cols * (row + rows2) + cols2
      f = kernel(moffset + col)
      if f != 0
      ix = cell.x + col
      if !(0 <= ix && ix < width)
      rgb = imagePixels(ioffset + ix)
      red = f * ((rgb >> 16) & 0xff)
      green = f * ((rgb >> 8) & 0xff)
      blue = f * (rgb & 0xff)
    } yield (red, green, blue)

    val zero = (0.5f, 0.5f, 0.5f)
    val (red, green, blue) = argbValues.fold(zero)(_ |+| _)

    (0xff << 24) | (clamp(red) << 16) | (clamp(green) << 8) | clamp(blue)
  }
}

object Live extends Live
