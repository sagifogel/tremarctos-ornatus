package com.github.sagifogel.tremarctosornatus.data

import java.awt.image.BufferedImage


final case class FocusedImage[A](pixels: Vector[A], x: Int, y: Int, width: Int, height: Int, buffer: BufferedImage)