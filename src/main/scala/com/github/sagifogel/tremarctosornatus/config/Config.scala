package com.github.sagifogel.tremarctosornatus.config

import io.circe
import io.circe.config.parser
import io.circe.generic.auto._

final case class Convolution(size: Int, weight: Int)
final case class AppSettings(imagePath: String,
                             convolution: Convolution)

trait Config {
  val config: Either[circe.Error, AppSettings]
}

object Config {
  trait Live extends Config {
    val config: Either[circe.Error, AppSettings] = parser.decode[AppSettings]
  }

  object Live extends Live
}
