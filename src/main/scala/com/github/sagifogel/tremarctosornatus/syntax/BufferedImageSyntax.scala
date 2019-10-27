package com.github.sagifogel.tremarctosornatus.syntax

import java.awt.image.BufferedImage

import cats.instances.int._
import cats.syntax.eq._
import com.github.sagifogel.tremarctosornatus.data.FocusedImage

object BufferedImageSyntax {
  implicit class BufferedImageOps(val bufferedImage: BufferedImage) extends AnyVal {
    def toFocusedImage: FocusedImage[Int] = {
      val width = bufferedImage.getWidth
      val height = bufferedImage.getHeight
      val argbVector = bufferedImage.getARGB(0, 0).toVector

      FocusedImage[Int](argbVector, 0, 0, width, height, bufferedImage)
    }

    def fromVector(rgbs: Vector[Int], x: Int, y: Int): BufferedImage = {
      val rgbArray = rgbs.toArray
      val width = bufferedImage.getWidth
      val height = bufferedImage.getHeight
      val colorModel = bufferedImage.getColorModel
      val raster = colorModel.createCompatibleWritableRaster(bufferedImage.getWidth, bufferedImage.getHeight)
      val newBufferedImage = new BufferedImage(colorModel, raster, colorModel.isAlphaPremultiplied, null)
      val imageType = bufferedImage.getType

      if (imageType === BufferedImage.TYPE_INT_ARGB || imageType === BufferedImage.TYPE_INT_RGB)
        newBufferedImage.getRaster.setDataElements(x, y, width, height, rgbArray)
      else
        newBufferedImage.setRGB(x, y, width, height, rgbArray, 0, width)

      newBufferedImage
    }

    def getARGB(x: Int, y: Int): Array[Int] = {
      val width = bufferedImage.getWidth
      val height = bufferedImage.getHeight
      val imageType = bufferedImage.getType
      val pixels = Array.fill(width * height)(0)

      if (imageType === BufferedImage.TYPE_INT_ARGB || imageType === BufferedImage.TYPE_INT_RGB)
        bufferedImage.getRaster.getDataElements(x, y, width, height, pixels).asInstanceOf[Array[Int]]
      else
        bufferedImage.getRGB(x, y, width, height, pixels, 0, width)
    }
  }
}
