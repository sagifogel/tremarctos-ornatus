package com.github.sagifogel.tremarctosornatus.syntax

import cats.implicits._
import cats.syntax.eq._

import java.awt.image.BufferedImage

object BufferedImageSyntax {

  implicit class BufferedImageOps(val bufferedImage: BufferedImage) extends AnyVal {
    def fromArray[T](data: Array[T]): BufferedImage = {
      val colorModel = bufferedImage.getColorModel
      val raster = colorModel.createCompatibleWritableRaster(bufferedImage.getWidth, bufferedImage.getHeight)

      new BufferedImage(colorModel, raster, colorModel.isAlphaPremultiplied, null)
    }

    def getARGB(x: Int, y: Int, height: Int, width: Int): Array[Int] = {
      val imageType = bufferedImage.getType
      val pixels = Array.range(0, width * height)

      if (imageType === BufferedImage.TYPE_INT_ARGB || imageType === BufferedImage.TYPE_INT_RGB)
        bufferedImage.getRaster.getDataElements(x, y, width, height, pixels).asInstanceOf[Array[Int]]
      else
        bufferedImage.getRGB(x, y, width, height, pixels, 0, width)
    }
  }
}
