package com.github.sagifogel.tremarctosornatus

import cats.implicits._

import java.awt.image.BufferedImage

final case class Kernel(length: Int, weight: Int, matrix: Array[Double])

object Gaussian {
  private def createKernel(length: Int, weight: Int): Kernel = {
    val kernel = Array.fill(length)(Array.fill[Double](length)(elem = 0))
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

  def convolve(image: FocusedImage[Byte]): Int = {
    process(image, createKernel(19, 9))
  }

  def getARGB(bufferedImage: BufferedImage, x: Int, y: Int, height: Int, width: Int): Array[Int] = {
    val imageType = bufferedImage.getType
    val pixels = Array.range(0, width * height)

    if (imageType === BufferedImage.TYPE_INT_ARGB || imageType === BufferedImage.TYPE_INT_RGB) {
      bufferedImage.getRaster.getDataElements(x, y, width, height, pixels).asInstanceOf[Array[Int]]
    } else bufferedImage.getRGB(x, y, width, height, pixels, 0, width)
  }

  private def clamp(color: Double): Int = {
    val value = color.toInt
    if (value < 0) 0
    else if (value > 255) 255
    else value
  }

  def process(cell: FocusedImage[Byte], kernel: Kernel): Int = {
    val rows = kernel.length
    val cols = kernel.length
    val rows2 = rows / 2
    val cols2 = cols / 2
    val height = cell.buffer.getHeight
    val width = cell.buffer.getWidth
    val matrix = kernel.matrix
    val imagePixels = getARGB(cell.buffer, 0, 0, height, width)
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
