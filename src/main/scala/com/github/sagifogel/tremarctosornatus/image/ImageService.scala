package com.github.sagifogel.tremarctosornatus.image

import java.awt.image.BufferedImage
import java.io.IOException
import java.nio.file.Paths

import com.github.sagifogel.tremarctosornatus.Main.AppEnvironment
import com.github.sagifogel.tremarctosornatus.config.AppSettings
import com.github.sagifogel.tremarctosornatus.image.ImageService.Live
import javax.imageio.ImageIO
import zio.ZIO
import zio.blocking.effectBlocking
import zio.console.Console

import scala.{Function => F}

trait ImageService {
  val image: ImageService.Service
}

final case class SavedFile(filePath: String, extension: String)

object ImageService {
  trait Service {
    def readImage(config: AppSettings): ZIO[AppEnvironment, IOException, BufferedImage]

    def writeImage(config: AppSettings, buffer: BufferedImage):
    ZIO[AppEnvironment, IOException, Unit]
  }

  trait Live extends ImageService {
    val image: Service = new Service {
      override def readImage(config: AppSettings): ZIO[AppEnvironment, IOException, BufferedImage] = {
        val file = Paths.get(config.convolution.imagePath).toFile

        effectBlocking(ImageIO.read(file)).refineToOrDie[IOException]
      }

      override def writeImage(config: AppSettings, buffer: BufferedImage): ZIO[AppEnvironment, IOException, Unit] = {
        for {
          console <- ZIO.access[Console](_.console)
          _ <- effectBlocking {
            val savedFile = resolveNewFileName(config.convolution.imagePath)
            val file = Paths.get(savedFile.filePath).toFile

            ImageIO.write(buffer, savedFile.extension, file)
          }.refineToOrDie[IOException].flatMap(F.const(console.putStrLn("Image has been saved")))
        } yield ()
      }

      private def resolveNewFileName(imagePath: String): SavedFile = {
        val extensionIndex = imagePath.lastIndexOf('.')
        val (fileName, extension) = imagePath.splitAt(extensionIndex)

        SavedFile(s"$fileName-blur$extension", extension.replaceFirst(".", ""))
      }
    }
  }
}

object Live extends Live
