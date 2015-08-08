import java.awt.image.{BufferedImage, WritableRaster, Raster}
import java.io.File
import javax.imageio.ImageIO

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object PIxelSorter extends App{



  def loadImage(f: File): BufferedImage = javax.imageio.ImageIO.read(f)

  def sortPixelsY(image: BufferedImage, sort: SortingFunctons.Pixel => Float): BufferedImage = {
    println("Sorting by Y")
    val width = image.getWidth
    val height = image.getHeight
    val sortable = image.getWritableTile(width,height)
    val sorted: BufferedImage = new BufferedImage(width,height,image.getType)
    for {
      x <- sortable.getMinX to (width- 1)
    } yield {
      println(x)
      val line = sortable.getPixels(x, 0, 1, height-1, new Array[Float](height * 3))
      sorted.getRaster.setPixels(x, 0, 1, height-1, sortLine(line,sort))
    }
    sorted
  }
  def sortPixelsX(image: BufferedImage, sort: SortingFunctons.Pixel => Float): BufferedImage = {
    println("Sorting by X")
    val width = image.getWidth
    val height = image.getHeight
    val sortable = image.getWritableTile(width,height)
    val sorted: BufferedImage = new BufferedImage(width,height,image.getType)
    for {
      y <- sortable.getMinY to (height - 1)
    } yield {
      println(y)
      val line = sortable.getPixels(0, y , width-1, 1, new Array[Float](width * 3))
      sorted.getRaster.setPixels(0, y , width-1, 1, sortLine(line,sort))
    }
    sorted
  }
  def sortLine(pixels: Array[Float], sort: SortingFunctons.Pixel => Float ): Array[Float] ={
    val linePixels = pixels.grouped(3).map( pixel => (pixel(0),pixel(1),pixel(2))).toList
    linePixels.map{
      case pixel => (pixel,SortingFunctons.weightedSum(pixel))
    }.sortWith( _._2 < _._2 )
      .map{ case (pixel,variable) => List(pixel._1, pixel._2, pixel._3)}.flatten.toArray
  }

  object SortingFunctons {
    def reversed(a: Float, b: Float): Boolean = a > b
    def standard(a: Float, b: Float): Boolean = a < b

    type Pixel = (Float, Float, Float)

    def weightedSum(pixel: Pixel): Float = pixel._1*10 + pixel._2 + pixel._3*20
    def sum(pixel: Pixel): Float = pixel._1 + pixel._2 + pixel._3
    def max(pixel: Pixel) = Seq(pixel._1,pixel._2,pixel._3).max
    def vibrancy(pixel: Pixel) = Seq(pixel._1,pixel._2,pixel._3).max - Seq(pixel._1,pixel._2,pixel._3).min
  }

  def getResource(name: String) = new File(getClass.getResource(name).toURI)
  val images = Array("/breakfast.jpg","/clouds.jpg","/flowers.jpg","/library.jpg","/mountains.jpg","/ocean-island.jpg")
                    .map(getResource)
  val compute = Future.sequence(images.map{ file =>
    Future {
      println(file.getName)
      val sorted = sortPixelsY(loadImage(file), SortingFunctons.vibrancy)
      if (ImageIO.write(sorted, "jpeg", new File("sorted-" + file.getName + ".jpg"))) {
        println("Output written to ")
      }
      else {
        println("Failed to write ")
      }
    }
  }.toList)

  Await.ready(compute,Duration.Inf)

}
