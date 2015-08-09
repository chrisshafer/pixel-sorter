import java.awt.image.{BufferedImage, WritableRaster, Raster}
import java.io.{FileFilter, File}
import javax.imageio.ImageIO

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object PIxelSorter extends App{

  implicit val sortConfig = new SortConfig(SortingFunctons.square, { length => length })

  case class SortConfig(sortingFunction: Pixel => Float, splitSize: (Int) => Int)
  case class Pixel(red: Float, green: Float, blue: Float){
    def max: Float = this match {
      case x if x.red > x.green & x.red > x.blue => x.red
      case y if y.green > y.red & y.green > y.blue => y.green
      case z if z.blue > z.red & z.blue > z.green => z.blue
      case equal => equal.red
    }
    def min: Float = this match {
      case x if x.red < x.green & x.red < x.blue => x.red
      case y if y.green < y.red & y.green < y.blue => y.green
      case z if z.blue < z.red & z.blue < z.green => z.blue
      case equal => equal.red
    }
    def purecolor: Pixel = Pixel( red - this.min, green - this.min, blue - this.min)
  }
  object Pixel {
    def apply( tup3: (Float,Float,Float)) = new Pixel(tup3._1,tup3._2,tup3._3)
  }

  def sortPixelsY(image: BufferedImage)(implicit config: SortConfig): BufferedImage = {
    val width = image.getWidth
    val height = image.getHeight
    val sortable = image.getWritableTile(width,height)
    val sorted: BufferedImage = new BufferedImage(width,height,image.getType)
    for {
      x <- sortable.getMinX to (width- 1)
    } yield {
      if (x%100 == 0) println(x*100/width+"%")
      val line = sortable.getPixels(x, 0, 1, height-1, new Array[Float](height * 3))
      sorted.getRaster.setPixels(x, 0, 1, height-1, sortLine(line))
    }
    sorted
  }
  def sortPixelsX(image: BufferedImage)(implicit config: SortConfig): BufferedImage = {
    val width = image.getWidth
    val height = image.getHeight
    val sortable = image.getWritableTile(width,height)
    val sorted: BufferedImage = new BufferedImage(width,height,image.getType)
    for {
      y <- sortable.getMinY to (height - 1)
    } yield {
      if (y%100 == 0) println(y*100/height+"%")
      val line = sortable.getPixels(0, y , width-1, 1, new Array[Float](width * 3))
      sorted.getRaster.setPixels(0, y , width-1, 1, sortLine(line))
    }
    sorted
  }
  def sortLine(pixels: Array[Float])(implicit config: SortConfig): Array[Float] ={
    val linePixels = pixels.grouped(3).map( pixel => (pixel(0),pixel(1),pixel(2))).toList
    linePixels.map{
      case pixel => (pixel,config.sortingFunction(Pixel(pixel)))
    }.grouped(config.splitSize(pixels.length)).map(_.sortWith( _._2 > _._2 )).flatten
      .map{ case (pixel,variable) => List(pixel._1, pixel._2, pixel._3)}.flatten.toArray
  }

  object SortingFunctons {
    def weightedSquare(pixel: Pixel): Float =
      pixel.red*pixel.red*256 + pixel.green*pixel.green*512 + pixel.blue*pixel.blue*768
    def square(pixel: Pixel): Float = pixel.red*pixel.red + pixel.green*pixel.green + pixel.blue*pixel.blue
    def sum(pixel: Pixel): Float = pixel.red + pixel.green + pixel.blue
    def weightedSum(pixel: Pixel): Float = pixel.red*256 + pixel.green*512 + pixel.blue*768
    def colorWeightedSum(pixel: Pixel): Float =
      pixel.purecolor.red * 256 + pixel.purecolor.green * 512 + pixel.purecolor.blue*768 + weightedSum(pixel)
    def max(pixel: Pixel): Float = pixel.max
    def vibrancy(pixel: Pixel): Float = pixel.max - pixel.min
  }

  def loadImage(f: File): BufferedImage = javax.imageio.ImageIO.read(f)
  def getResource(name: String) = new File(getClass.getResource(name).toURI)

  val imagesInResources = getResource("/").listFiles().filter{ case file =>
    file.getName.toLowerCase.endsWith(".jpg") || file.getName.toLowerCase.endsWith(".png")}
  val compute = Future.sequence(imagesInResources.map{ file =>
    Future {
      println("Processing : "+file.getName)
      val sorted = sortPixelsY(loadImage(file))
      if (ImageIO.write(sorted, "jpeg", new File("sorted-" + file.getName + ".jpg"))){
        println("Finished processing : "+file.getName)
      } else println("Failed to write ")
    }
  }.toList)

  Await.ready(compute,Duration.Inf)

}
