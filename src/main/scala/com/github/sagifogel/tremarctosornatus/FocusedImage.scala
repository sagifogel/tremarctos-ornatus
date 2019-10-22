package com.github.sagifogel.tremarctosornatus

import java.awt.image.BufferedImage

final case class FocusedImage[A](pixels: Array[A], x: Int, y: Int, buffer: BufferedImage)