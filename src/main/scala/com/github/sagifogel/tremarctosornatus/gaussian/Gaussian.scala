package com.github.sagifogel.tremarctosornatus.gaussian

import java.awt.image.BufferedImage

import cats.Comonad
import cats.implicits._
import com.github.sagifogel.tremarctosornatus.Main.AppEnvironment
import com.github.sagifogel.tremarctosornatus.config.AppSettings
import com.github.sagifogel.tremarctosornatus.data.FocusedImage
import com.github.sagifogel.tremarctosornatus.gaussian.Gaussian.Live
import com.github.sagifogel.tremarctosornatus.image.ImageService
import com.github.sagifogel.tremarctosornatus.syntax.BufferedImageSyntax._
import zio.{RIO, UIO, ZIO}

final case class Kernel(length: Int, weight: Int, matrix: Array[Double])

trait Gaussian {
  val gaussian: Gaussian.Service
}

object Gaussian {
  trait Service {
    def convolve(config: AppSettings)(implicit WA: Comonad[FocusedImage]): RIO[AppEnvironment, BufferedImage]
  }

  trait Live extends Gaussian {
    override val gaussian: Service = new Service {
      override def convolve(config: AppSettings)
                           (implicit WA: Comonad[FocusedImage]): RIO[AppEnvironment, BufferedImage] =
        for {
          imageService <- ZIO.access[ImageService](_.image)
          focusedImage <- imageService.readImage(config).map(_.toFocusedImage)
          convolution = config.convolution
          kernel <- createKernel(convolution.size, convolution.weight)
          convolutedImage <- ZIO.effect(focusedImage.coflatMap(process(_, kernel)))
          pixels = convolutedImage.pixels.toArray
        } yield convolutedImage.buffer.fromArray(pixels, 0, 0)
    }
  }

  private def createKernel(length: Int, weight: Int): UIO[Kernel] = UIO {
    val kernel = Array.fill(length)(Array.fill(length)(elem = 0d))
    val lines = (length - 1) / 2
    val constant = 1d / (2 * Math.PI * weight * weight)

    for {
      x <- -lines until lines
      y <- -lines until lines
      distance = ((y * y) + (x * x)) / (2 * weight * weight)
      storedValue = constant * Math.exp(-distance.toDouble)
      _ = kernel(x)(y) = storedValue
    } yield ()

    val kernelSum = kernel.foldLeft(0d)((accum, arr) => arr.sum + accum)

    for {
      x <- 0 until length
      y <- 0 until length
      storedValue = kernel(x)(y)
      _ = kernel(x)(y) = storedValue * 1d / kernelSum
    } yield ()

    Kernel(length, weight, kernel.flatten)
  }

  private def clamp(color: Double): Int = {
    val value = color.toInt
    if (value < 0) 0
    else if (value > 255) 255
    else value
  }

  def process(cell: FocusedImage[Int], kernel: Kernel): Int = {
    val rows = kernel.length
    val cols = kernel.length
    val rows2 = rows / 2
    val cols2 = cols / 2
    val height = cell.buffer.getHeight
    val width = cell.buffer.getWidth
    val matrix = kernel.matrix
    val imagePixels = cell.pixels
    val argbValues = for {
      row <- -rows2 to rows2
      col <- -cols2 to cols2
      iy = cell.y + row
      ioffset = iy * width
      if 0 <= iy && iy < height
      moffset = cols * (row + rows2) + cols2
      f = matrix(moffset + col)
      if f != 0
      ix = cell.x + col
      rgb = imagePixels(ioffset + ix)
      red = f * ((rgb >> 16) & 0xff)
      green = f * ((rgb >> 8) & 0xff)
      blue = f * (rgb & 0xff)
    } yield (red, green, blue)

    val zero = (0.5d, 0.5d, 0.5d)
    val (red, green, blue) = argbValues.fold(zero)(_ |+| _)

    (0xff << 24) | (clamp(red) << 16) | (clamp(green) << 8) | clamp(blue)
  }
}

object Live extends Live
