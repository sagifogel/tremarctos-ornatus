package com.github.sagifogel.tremarctosornatus

import com.github.sagifogel.tremarctosornatus.config.Config
import com.github.sagifogel.tremarctosornatus.gaussian.Gaussian
import com.github.sagifogel.tremarctosornatus.image.ImageService
import com.github.sagifogel.tremarctosornatus.instances.ComonadInstances._
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console._
import zio.random.Random
import zio.system.System

import scala.{Function => F}

object Main extends App {
  type AppEnvironment = ZEnv with Config with Gaussian with ImageService

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    val environment: AppEnvironment = new Config.Live
      with Gaussian.Live with Clock.Live with Console.Live
      with System.Live with Random.Live with Blocking.Live with ImageService.Live

    program.provideSome(F.const(environment))
      .foldM(ex => putStrLn(s"Execution failed with: $ex") *> ZIO.succeed(1), F.const(ZIO.succeed(0)))
  }

  private val program: ZIO[AppEnvironment, Throwable, Unit] =
    for {
      config <- ZIO.access[Config](_.config).flatMap(ZIO.fromEither(_))
      gaussian <- ZIO.access[Gaussian](_.gaussian)
      imageService <- ZIO.access[ImageService](_.image)
      buffer <- imageService.readImage(config)
      output <- gaussian.convolve(config, buffer)
      _ <- imageService.writeImage(config, output)
    } yield ()
}
