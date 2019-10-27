package com.github.sagifogel.tremarctosornatus.gaussian

import java.awt.image.BufferedImage

import cats.CoflatMap
import cats.implicits._
import com.github.sagifogel.tremarctosornatus.config.AppSettings
import com.github.sagifogel.tremarctosornatus.data.FocusedImage
import com.github.sagifogel.tremarctosornatus.gaussian.Gaussian.Live
import com.github.sagifogel.tremarctosornatus.syntax.BufferedImageSyntax._
import zio.{Task, ZIO}

final case class Kernel(width: Int, height: Int, data: Vector[Float])

trait Gaussian {
  val gaussian: Gaussian.Service
}

object Gaussian {
  trait Service {
    def convolve(config: AppSettings, buffer: BufferedImage)
                (implicit WA: CoflatMap[FocusedImage]): Task[BufferedImage]
  }

  trait Live extends Gaussian {
    override val gaussian: Service = new Service {
      override def convolve(config: AppSettings, buffer: BufferedImage)
                           (implicit WA: CoflatMap[FocusedImage]): Task[BufferedImage] = {
        val convolution = config.convolution
        val radius = convolution.radius
        val kernel = createKernel(convolution.radius)
        val focusedImage = buffer.toFocusedImage
        val width = focusedImage.width
        val height = focusedImage.height
        val process = filter(_, kernel, radius)

        for {
          convolutedImage <- ZIO.effect(focusedImage.coflatMap(process))
          transposedImage = convolutedImage.copy(height = width, width = height)
          gaussianImage <- ZIO.effect(transposedImage.coflatMap(process))
        } yield gaussianImage.buffer.fromVector(convolutedImage.pixels, 0, 0)
      }
    }
  }

  private def createKernel(radius: Float): Kernel = {
    val roundRadius = radius.ceil.toInt
    val rows = roundRadius * 2 + 1
    val sigma = radius / 3
    val sigma22 = 2 * sigma * sigma
    val sigmaPi2 = 2f * Math.PI.toFloat * sigma
    val sqrtSigmaPi2 = Math.sqrt(sigmaPi2).toFloat
    val radius2 = radius * radius
    val kernelValues = for {
      row <- -roundRadius to roundRadius
      distance = (row * row).toFloat
      value = if (distance > radius2) 0f
              else Math.exp(-distance / sigma22).toFloat / sqrtSigmaPi2
    } yield value.toFloat

    val total = kernelValues.sum
    val data = kernelValues.foldLeft(Vector.empty[Float])(_ :+ _ / total)

    Kernel(rows, 1, data)
  }

  private def clamp(color: Float): Int = {
    val value = color.toInt
    if (value < 0) 0
    else if (value > 255) 255
    else value
  }

  private def filter(focus: FocusedImage[Int], kernel: Kernel, radius: Float): Int = {
    if (radius === 0) 0
    else {
      val matrix = kernel.data
      val width = focus.width
      val cols2 = kernel.width / 2
      val ioffset = focus.y * width
      val moffset = cols2
      val rgbs =
        for {
          col <- -cols2 until cols2
          f = matrix(moffset + col)
          if f =!= 0
          xcol = focus.x + col
          ix = if (xcol < 0) 0
          else if (xcol >= width) width - 1
          else xcol
          rgb = focus.pixels(ioffset + ix)
          pa = (rgb >> 24) & 0xff
          pr = (rgb >> 16) & 0xff
          pg = (rgb >> 8) & 0xff
          pb = rgb & 0xff
          a = f * pa
          r = f * pr
          g = f * pg
          b = f * pb
        } yield (a, r, g, b)

      val (a, r, b, g) = rgbs.foldLeft((0.5f, 0.5f, 0.5f, 0.5f))(_ |+| _)
      val ia = clamp(a)
      val ir = clamp(r)
      val ig = clamp(g)
      val ib = clamp(b)

      (ia << 24) | (ir << 16) | (ig << 8) | ib
    }
  }
}

object Live extends Live
