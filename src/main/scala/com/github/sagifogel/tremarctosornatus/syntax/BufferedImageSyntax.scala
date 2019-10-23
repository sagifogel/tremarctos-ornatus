package com.github.sagifogel.tremarctosornatus.syntax

import cats.syntax.eq._
import cats.instances.int._
import java.awt.image.BufferedImage

import com.github.sagifogel.tremarctosornatus.data
import com.github.sagifogel.tremarctosornatus.data.FocusedImage

object BufferedImageSyntax {
  implicit class BufferedImageOps(val bufferedImage: BufferedImage) extends AnyVal {
    def toFocusedImage: FocusedImage[Int] = {
      data.FocusedImage[Int](bufferedImage.getARGB(0, 0).toVector, 0, 0, bufferedImage)
    }
    def fromArray(rgbArray: Array[Int], x: Int, y: Int): BufferedImage = {
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
      val pixels = Array.range(0, width * height)

      if (imageType === BufferedImage.TYPE_INT_ARGB || imageType === BufferedImage.TYPE_INT_RGB)
        bufferedImage.getRaster.getDataElements(x, y, width, height, pixels).asInstanceOf[Array[Int]]
      else
        bufferedImage.getRGB(x, y, width, height, pixels, 0, width)
    }
  }
}
