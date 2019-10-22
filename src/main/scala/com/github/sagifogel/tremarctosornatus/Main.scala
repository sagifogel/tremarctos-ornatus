package com.github.sagifogel.tremarctosornatus

import com.github.sagifogel.tremarctosornatus.config.Config
import io.circe
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console._
import zio.random.Random
import zio.system.System

import scala.{ Function => F }

object Main extends App {
  type AppEnvironment = ZEnv with Config

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    val environment: AppEnvironment = new Config.Live
      with Clock.Live with Console.Live
      with System.Live with Random.Live with Blocking.Live

    program.provideSome(F.const(environment))
      .foldM(ex => putStrLn(s"Execution failed with: $ex") *> ZIO.succeed(1), F.const(ZIO.succeed(1)))
  }

  private val program: ZIO[AppEnvironment, circe.Error, Unit] = for {
    _ <- ZIO.access[Config](_.config).flatMap(ZIO.fromEither)
  } yield ()
}
