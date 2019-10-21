package com.github.sagifogel.tremarctosornatus

import java.awt.image.BufferedImage

final case class FocusedImage[A](vector: Vector[A], x: Int, y: Int, buffer: BufferedImage)