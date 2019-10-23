package com.github.sagifogel.tremarctosornatus.syntax

import com.github.sagifogel.tremarctosornatus.data.FocusedImage

object FocusedImageSyntax {
  implicit class FocusedImageOps[A](val focusedImage: FocusedImage[A]) extends AnyVal {
    def index: Int = {
      val buffer = focusedImage.buffer
      buffer.getWidth * focusedImage.y + focusedImage.x
    }
  }
}
