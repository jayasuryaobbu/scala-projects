package controllers

import java.nio.file.{Files, Paths}
import java.io.File

import java.awt.Graphics2D

import java.awt._
import java.awt.image.BufferedImage
import java.io._
import javax.imageio.ImageIO
import javax.swing.JFrame


import javax.inject._
import play.api.mvc.MultipartFormData.FilePart
import play.api.mvc._
import play.api.i18n.Messages._

import scala.reflect.internal.util.NoFile.input

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {


  def index(): Action[AnyContent] =Action { implicit request =>
    Ok(views.html.index())
  }

  def makeGray(testImage: java.awt.image.BufferedImage): java.awt.image.BufferedImage = {
    val w = testImage.getWidth
    val h = testImage.getHeight
    for {
      w1 <- (0 until w).toVector
      h1 <- (0 until h).toVector
    } yield {
      val col = new Color(testImage.getRGB(w1, h1))
      // val R = (col & 0xff0000) / 65536
      // val G = (col & 0xff00) / 256
      // val B = (col & 0xff)
      val R = (col.getRed * 0.299).toInt
      val G = (col.getGreen * 0.587).toInt
      val B = (col.getBlue * 0.114).toInt
      val graycol = (R + G + B) / 3
      testImage.setRGB(w1, h1, new Color(graycol, graycol, graycol).getRGB)
    }
    testImage
  }

  def makeColor(grayImage: java.awt.image.BufferedImage): java.awt.image.BufferedImage = {

    val w = grayImage.getWidth
    val h = grayImage.getHeight

    val redImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);

    for {
      w1 <- (0 until w).toVector
      h1 <- (0 until h).toVector
    } yield {
      val col = new Color(grayImage.getRGB(w1, h1))
      val red = col.getRed
      val green = col.getGreen
      val blue = col.getBlue
      val R = if (red > 127) { 255 } else { red/2 }
      val G = if (green > 127) { 255 } else { green/2 }
      val B = if (blue > 127) { 255 } else { blue/2 }
      val redColor = new Color(R, G, B).getRGB
      redImage.setRGB(w1, h1, redColor);
    }
  redImage
  }


  def imageColorizer() = Action(parse.multipartFormData(maxLength = 1024 * 1000)) { implicit request =>

    val photo = request.body.file("bwtocolor").map {
      case FilePart(key, filename, contentType, file, fileSize, dispositionType) =>
        val bytes = Files.readAllBytes(file.toPath)
        (bytes, file)
    }

    Files.write(Paths.get("public/images/photo.png"), photo.get._1)

    val testImage = ImageIO.read(new File("public/images/photo.png"))
    val grayImage = makeGray(testImage)
    ImageIO.write(grayImage, "png", new File("public/images/photo1.png"))



    Ok(views.html.index())
  }


}
