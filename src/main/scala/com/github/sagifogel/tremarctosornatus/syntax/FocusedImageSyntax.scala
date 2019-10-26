package com.github.sagifogel.tremarctosornatus.syntax

import com.github.sagifogel.tremarctosornatus.data.FocusedImage

object FocusedImageSyntax {
  implicit class FocusedImageOps[A](val focusedImage: FocusedImage[A]) extends AnyVal {
    def index: Int = {
      focusedImage.width * focusedImage.y + focusedImage.x
    }
  }
}
