import java.awt.image.BufferedImage
import java.awt.{Graphics, Color}
import java.io.{PrintStream, File}
import javax.imageio.ImageIO
import scala.util.Random

object EvolutionaryPainting extends App {

  val rand = new Random()

  val iterations = 999999
  val rRatio = 0.8
  val initShapes = 1
  val addFreq = 0.02
  val delFreq = 0.4
  val mutFreq = 0.1
  val mutCoordRatio = 0.33
  val mutRRatio = 0.45
  val mutColorRatio = 0.4
  val mutAlphaRatio = 0.2
  val minAlpha = 10
  val maxAlpha = 80
  val maxShapes = 50
  val (minPoly, maxPoly) = (3, 9)

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
      val n = xs.length

      lazy val xs2 = xs.map(x =>
        if(rand.nextInt(n) != 0) x
        else x + ((rand.nextGaussian() - 0.5) * diagonal * mutCoordRatio).toInt
      )
      lazy val ys2 = ys.map(y =>
        if(rand.nextInt(n) != 0) y
        else y + ((rand.nextGaussian() - 0.5) * diagonal * mutCoordRatio).toInt
      )
      lazy val cMut = new Color(
        (c.getRed + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (c.getGreen + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (c.getBlue + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (c.getAlpha + ((rand.nextGaussian() - 0.5) * (maxAlpha - minAlpha) * mutAlphaRatio).toInt).max(minAlpha).min(maxAlpha)
      )
      if(rand.nextInt(2) == 0) copy(xs2, ys2, c)
      else copy(xs, ys, cMut)
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
    def randomDel(dna: DNA): DNA = {
      val n = dna.size
      dna.filter(_ => rand.nextInt(n) > 0)
    }
    def randomAdd(dna: DNA): DNA =
      makeOne +: (if(dna.size < maxShapes) dna else randomDel(randomDel(dna)))
    def randomMut(dna: DNA): DNA =
      dna.map(c => if(rand.nextDouble() < mutFreq) c.mutated else c).toVector
    def randomShuffle(dna: DNA): DNA =
      rand.shuffle(dna).toVector

    def mutate(dna: DNA): DNA = {
      val ops: List[(Double, DNA => DNA)] =
        List(delFreq -> randomDel, addFreq -> randomAdd, mutFreq -> randomMut)

      val opsOpts = ops.map {
        case (freq, f) => if (rand.nextDouble() < freq) Some(f) else None
      }

      var result = dna
      opsOpts.foreach(fOpt => fOpt.foreach(f => result = f(result)))
      result
    }

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
    def fitness(dnaImg: BufferedImage): Int = {
      var total = 0
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

    val imagesWithFitness = (1 to 4).par.map { f =>
      val child = DNA.mutate(parent)
      val image = DNA.draw(child)
      val fitness = DNA.fitness(image)
      (child, image, fitness)
    }.toList

    val (bestChild, bestChildImg, bestChildFitness) = imagesWithFitness.sortBy(_._3).head

    if(bestChildFitness < parentFitness) {
      parentFitness = bestChildFitness
      out.println(s"$strIter $parentFitness ${parent.size}")
      parent = bestChild
      if(iter < 1000 || iter / 100 > lastImgDump) {
        ImageIO.write(bestChildImg, "png", new File(s"images/$strIter.png"))
        lastImgDump = iter / 100
      }

    }
  }

  println("finished")
}
