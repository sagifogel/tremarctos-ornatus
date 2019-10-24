package com.github.sagifogel.tremarctosornatus.config

import com.github.sagifogel.tremarctosornatus.config.Config.Live
import io.circe
import io.circe.config.parser
import io.circe.generic.auto._

final case class Convolution(radius: Float, imagePath: String)
final case class AppSettings(convolution: Convolution)

trait Config {
  val config: Either[circe.Error, AppSettings]
}

object Config {
  trait Live extends Config {
    val config: Either[circe.Error, AppSettings] = parser.decode[AppSettings]
  }
}

object Live extends Live
