import java.awt.image.{BufferedImage, WritableRaster, Raster}
import java.io.File
import javax.imageio.ImageIO

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object PIxelSorter extends App{


  case class Pixel(red: Float, green: Float, blue: Float)
  object Pixel {
    def apply( tup3: (Float,Float,Float)) = new Pixel(tup3._1,tup3._2,tup3._3)
  }

  def loadImage(f: File): BufferedImage = javax.imageio.ImageIO.read(f)

  def sortPixelsY(image: BufferedImage, categorizer: Pixel => Float): BufferedImage = {
    val width = image.getWidth
    val height = image.getHeight
    val sortable = image.getWritableTile(width,height)
    val sorted: BufferedImage = new BufferedImage(width,height,image.getType)
    for {
      x <- sortable.getMinX to (width- 1)
    } yield {
      println(x)
      val line = sortable.getPixels(x, 0, 1, height-1, new Array[Float](height * 3))
      sorted.getRaster.setPixels(x, 0, 1, height-1, sortLine(line,categorizer))
    }
    sorted
  }
  def sortPixelsX(image: BufferedImage, categorizer: Pixel => Float): BufferedImage = {
    val width = image.getWidth
    val height = image.getHeight
    val sortable = image.getWritableTile(width,height)
    val sorted: BufferedImage = new BufferedImage(width,height,image.getType)
    for {
      y <- sortable.getMinY to (height - 1)
    } yield {
      println(y)
      val line = sortable.getPixels(0, y , width-1, 1, new Array[Float](width * 3))
      sorted.getRaster.setPixels(0, y , width-1, 1, sortLine(line,categorizer))
    }
    sorted
  }
  def sortLine(pixels: Array[Float], categorizer: Pixel => Float ): Array[Float] ={
    val linePixels = pixels.grouped(3).map( pixel => (pixel(0),pixel(1),pixel(2))).toList
    linePixels.map{
      case pixel => (pixel,categorizer(Pixel(pixel)))
    }.sortWith( _._2 > _._2 )
      .map{ case (tuprgb,variable) => List(tuprgb._1, tuprgb._2, tuprgb._3)}.flatten.toArray
  }

  object SortingFunctons {
    def weightedSum(pixel: Pixel): Float = pixel.red*10 + pixel.green + pixel.blue*20
    def sum(pixel: Pixel): Float = pixel.blue + pixel.green + pixel.blue
    def max(pixel: Pixel) = Seq(pixel.blue,pixel.green,pixel.blue).max
    def vibrancy(pixel: Pixel) = Seq(pixel.blue,pixel.green,pixel.blue).max - Seq(pixel.blue,pixel.green,pixel.blue).min
  }

  def getResource(name: String) = new File(getClass.getResource(name).toURI)
  val images = Array("/breakfast.jpg","/clouds.jpg","/flowers.jpg","/library.jpg","/mountains.jpg","/ocean-island.jpg")
    .map(getResource)
  val compute = Future.sequence(images.map{ file =>
    Future {
      println(file.getName)
      val sorted = sortPixelsY(loadImage(file), SortingFunctons.weightedSum)
      if (ImageIO.write(sorted, "jpeg", new File("sorted-" + file.getName + ".jpg"))) {
        println("Output written to ")
      } else println("Failed to write ")

    }
  }.toList)

  Await.ready(compute,Duration.Inf)

}
