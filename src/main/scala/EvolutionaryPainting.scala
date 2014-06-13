import java.awt.image.BufferedImage
import java.awt.{Graphics, Color}
import java.io.{PrintStream, File}
import javax.imageio.ImageIO
import scala.util.Random

object EvolutionaryPainting extends App {

  val rand = new Random()

  val iterations = 999999
  val rRatio = 0.8
  val initShapes = 0
  val addFreq = 0.22
  val delFreq = 0.21
  val mutFreq = 0.02
  val mutCoordRatio = 0.33
  val mutRRatio = 0.45
  val mutColorRatio = 0.15
  val mutAlphaRatio = 0.1
  val minAlpha = 10
  val maxAlpha = 60
  val maxShapes = 150
  val minPoly = 3
  val maxPoly = 12

  val img: BufferedImage = ImageIO.read(new File("monalisa.png"))
  val (w, h) = (img.getWidth, img.getHeight)
  val diagonal = math.sqrt(w*w + h*h).toInt / 4 * 3

  trait Shape {
    val c: Color

    def draw(g: Graphics): Unit
    def mutated: Shape
  }

//  case class Oval(x: Int, y: Int, r1: Int, r2: Int, c: Color) extends Shape {
//    def draw(g: Graphics) {
//      g.setColor(c)
//      g.fillOval(x, y, r1, r2)
//    }
//    def mutated: Oval = {
//      val xMut = x + ((rand.nextGaussian() - 0.5) * r2 * mutCoordRatio).toInt
//      val yMut = y + ((rand.nextGaussian() - 0.5) * r1 * mutCoordRatio).toInt
//      val r1Mut = r1 //+ ((rand.nextGaussian() - 0.5) * circle.r * mutRRatio).toInt
//      val r2Mut = r2
//      val cMut = new Color(
//        (c.getRed + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
//        (c.getGreen + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
//        (c.getBlue + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
//        (c.getAlpha + ((rand.nextGaussian() - 0.5) * (maxAlpha - minAlpha) * mutAlphaRatio).toInt).max(minAlpha).min(maxAlpha)
//      )
//      copy(
//        xMut.max(0).min(w - r1Mut),
//        yMut.max(0).min(h - r2Mut),
//        r1Mut.max(1),
//        r2Mut.max(1),
//        cMut)
//    }
//  }
//
//  object Oval {
//    def random = {
//      val rr1 = 1 + rand.nextInt((diagonal * rRatio).toInt)
//      val rr2 = 1 + rand.nextInt((diagonal * rRatio).toInt)
//      val rx = rand.nextInt(w - rr1)
//      val ry = rand.nextInt(h - rr2)
//      val rc = new Color(
//        rand.nextInt(256),
//        rand.nextInt(256),
//        rand.nextInt(256),
//        minAlpha + rand.nextInt(maxAlpha - minAlpha)
//      )
//      Oval(rx, ry, rr1, rr2, rc)
//    }
//  }


  case class Polygon(xs: Array[Int], ys: Array[Int], c: Color) extends Shape {
    def draw(g: Graphics) {
      g.setColor(c)
      g.fillPolygon(xs, ys, xs.length)
    }
    def mutated: Polygon = {
      val xs2 = xs.map(x => x + ((rand.nextGaussian() - 0.5) * diagonal * mutCoordRatio).toInt)
      val ys2 = ys.map(y => y + ((rand.nextGaussian() - 0.5) * diagonal * mutCoordRatio).toInt)
      val cMut = new Color(
        (c.getRed + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (c.getGreen + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (c.getBlue + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (c.getAlpha + ((rand.nextGaussian() - 0.5) * (maxAlpha - minAlpha) * mutAlphaRatio).toInt).max(minAlpha).min(maxAlpha)
      )
      copy(xs2, ys2, cMut)
    }
  }

  object Polygon {
    def random = {
      val n = rand.nextInt(maxPoly - minPoly) + minPoly
      val xs = Array.fill(n)(rand.nextInt(w))
      val ys = Array.fill(n)(rand.nextInt(h))
      val c = new Color(
        rand.nextInt(256),
        rand.nextInt(256),
        rand.nextInt(256),
        minAlpha + rand.nextInt(maxAlpha - minAlpha)
      )
      Polygon(xs, ys, c)
    }
  }

  type DNA = Vector[Shape]

  object DNA {
    def makeOne: Shape = Polygon.random
    def make: DNA = Vector.fill(initShapes)(makeOne)
    def randomDel(dna: DNA): DNA =
      if(dna.isEmpty) dna else dna.patch(rand.nextInt(dna.size), Nil, 1)
    def randomAdd(dna: DNA): DNA =
      makeOne +: (if(dna.size < maxShapes) dna else randomDel(dna))
    def randomMut(dna: DNA): DNA =
      dna.map(c => if(rand.nextDouble() < mutFreq) c.mutated else c).toVector
    def randomShuffle(dna: DNA): DNA =
      rand.shuffle(dna).toVector

    def draw(dna: DNA): BufferedImage = {
      val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
      val g = image.getGraphics
      g.setColor(Color.BLACK)
      g.fillRect(0, 0, w, h)
      dna.foreach { circle =>
        circle.draw(g)
      }
      image
    }
    def fitness(dnaImg: BufferedImage): Long = {
      var total: Long = 0L
      for(y <- 0 until h; x <- 0 until w) {
        val rgbImg = img.getRGB(x, y)
        val rgbDna = dnaImg.getRGB(x, y)
        val cImg = new Color(rgbImg)
        val cDna = new Color(rgbDna)
        val dRed = cImg.getRed - cDna.getRed
        val dGreen = cImg.getGreen - cDna.getGreen
        val dBlue = cImg.getBlue - cDna.getBlue
        val d = dRed * dRed + dGreen * dGreen + dBlue * dBlue
        total += d
      }
      total
    }
  }

  var parent = DNA.make
  val out = new PrintStream("plot.data")
  val pImg = DNA.draw(parent)
  var parentFitness = DNA.fitness(pImg)
  out.println(s"000000 ${DNA.fitness(pImg)}")
  ImageIO.write(pImg, "png", new File(s"images/000000.png"))
  var lastImgDump = 0

  for(iter <- 1 to iterations) {
    val strIter = "%06d".format(iter)

    val funs: List[DNA => DNA] = List(DNA.randomShuffle, DNA.randomDel, DNA.randomAdd, DNA.randomMut)
    val imagesWithFitness = funs.par.map { f =>
      val child = f(parent)
      val image = DNA.draw(child)
      val fitness = DNA.fitness(image)
      (child, image, fitness)
    }.toList

    val (bestChild, bestChildImg, bestChildFitness) = imagesWithFitness.sortBy(_._3).head

    if(bestChildFitness < parentFitness) {
      parentFitness = bestChildFitness
      out.println(s"$strIter $parentFitness ${parent.size}")
      parent = bestChild
      if(iter / 100 > lastImgDump) {
        ImageIO.write(bestChildImg, "png", new File(s"images/$strIter.png"))
        lastImgDump = iter / 100
      }

    }
  }

  println("finished")
}
