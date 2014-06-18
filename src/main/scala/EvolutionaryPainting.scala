import java.awt.image.BufferedImage
import java.awt.{Graphics, Color}
import java.io.{PrintStream, File}
import javax.imageio.ImageIO
import scala.util.Random

object EvolutionaryPainting extends App {

  if(args.length != 4) {
    println("usage: <exec> image_file output_dir log_file iterations")
    System.exit(1)
  }

  val img_file = args(0)
  val outdir = args(1)
  val plotfile = args(2)
  val iterations = args(3).toInt

  val rand = new Random()

  val initShapes = 1
  val shuffleFreq = 0.001
  val addFreq = 0.0006
  val delFreq = 0.001
  val mutFreq = 0.002
  val mutCoordRange = -20 to 20
  val mutRRange = -10 to 10
  val mutColorRange = -30 to 30
  val mutAlphaRange = -10 to 10
  val alphaRange = 10 to 60
  val maxShapes = 200
  val (minPoly, maxPoly) = (3, 10)
  val (polyAddFreq, polyDelFreq) = (0.005, 0.05)
  val (polyMutFreq, polyMutRange) = (0.1, -20 to 20)

  val img: BufferedImage = ImageIO.read(new File(img_file))
  val (w, h) = (img.getWidth, img.getHeight)

  trait Shape {
    val c: Color

    def draw(g: Graphics): Unit
    def mutated: Shape
  }

  case class Oval(x: Int, y: Int, r1: Int, r2: Int, c: Color) extends Shape {
    def draw(g: Graphics) {
      g.setColor(c)
      g.fillOval(x, y, r1, r2)
    }
    def mutated: Oval = {
      val xMut = x + mutCoordRange(rand.nextInt(mutCoordRange.size))
      val yMut = y + mutCoordRange(rand.nextInt(mutCoordRange.size))
      val r1Mut = r1 + mutRRange(rand.nextInt(mutRRange.size))
      val r2Mut = r2 + mutRRange(rand.nextInt(mutRRange.size))
      val cMut = new Color(
        (c.getRed + mutColorRange(rand.nextInt(mutColorRange.size))).max(0).min(255),
        (c.getGreen + mutColorRange(rand.nextInt(mutColorRange.size))).max(0).min(255),
        (c.getBlue + mutColorRange(rand.nextInt(mutColorRange.size))).max(0).min(255),
        (c.getAlpha + mutAlphaRange(rand.nextInt(mutAlphaRange.size))).max(alphaRange.head).min(alphaRange.last)
      )
      copy(xMut, yMut, r1Mut.max(1), r2Mut.max(1), cMut)
    }
  }

  object Oval {
    def random = {
      val rr1 = 1 + rand.nextInt(mutRRange.size)
      val rr2 = 1 + rand.nextInt(mutRRange.size)
      val rx = rand.nextInt(w)
      val ry = rand.nextInt(h)
      val rgb = new Color(img.getRGB(rx + rr1 >> 1, ry + rr2 >> 1))
      val rc = new Color(
        rgb.getRed, rgb.getGreen, rgb.getBlue,
        alphaRange(rand.nextInt(alphaRange.size))
      )
      Oval(rx, ry, rr1, rr2, rc)
    }
  }

  case class Polygon(points: Vector[(Int,Int)], c: Color) extends Shape {
    def draw(g: Graphics) {
      g.setColor(c)
      val (xs, ys) = points.unzip
      g.fillPolygon(xs.toArray, ys.toArray, points.size)
    }
    def mutated: Polygon = {
      val n = points.size

      val ps1: Vector[(Int,Int)] =
        if(polyDelFreq < rand.nextDouble() && n > minPoly)
          points.patch(rand.nextInt(n), Vector(), 1)
        else points

      val ps2 =
        if(polyAddFreq < rand.nextDouble() && ps1.size < maxPoly)
          points.patch(rand.nextInt(n), Vector((rand.nextInt(w), rand.nextInt(h))), 0)
        else
          ps1

      val ps3 = ps2.map{ case (x,y) => if(rand.nextDouble() < polyMutFreq) {
        (x + polyMutRange(rand.nextInt(polyMutRange.size)),
         y + polyMutRange(rand.nextInt(polyMutRange.size)))
      } else (x,y)}

      val cMut = new Color(
        (c.getRed + mutColorRange(rand.nextInt(mutColorRange.size))).max(0).min(255),
        (c.getGreen + mutColorRange(rand.nextInt(mutColorRange.size))).max(0).min(255),
        (c.getBlue + mutColorRange(rand.nextInt(mutColorRange.size))).max(0).min(255),
        (c.getAlpha + mutAlphaRange(rand.nextInt(mutAlphaRange.size))).max(alphaRange.head).min(alphaRange.last)
      )
      copy(ps3, cMut)
    }
  }

  object Polygon {
    def random = {
      val n = minPoly//rand.nextInt(maxPoly - minPoly) + minPoly
      val (sx, sy) = (rand.nextInt(w), rand.nextInt(h))
      val ps = (sx,sy) +: Vector.fill(n-1)(
        (sx + polyMutRange(rand.nextInt(polyMutRange.size)),
         sy + polyMutRange(rand.nextInt(polyMutRange.size)))
      )
      val rgb = new Color(img.getRGB(ps.head._1, ps.head._2))
      val c = new Color(
        rgb.getRed, rgb.getGreen, rgb.getBlue,
        alphaRange(rand.nextInt(alphaRange.size))
      )
      Polygon(ps, c)
    }
  }

  type DNA = Vector[Shape]

  object DNA {
    def makeOne: Shape = rand.nextInt(100) match {
        case n if n < 80 => Polygon.random
        case _ => Oval.random
      }

    def make: DNA = Vector.fill(initShapes)(makeOne)

    def randomDel(dna: DNA): DNA = {
      val n = dna.size
      dna.filter(_ => rand.nextInt(n) > 0)
    }

    def randomAdd(dna: DNA): DNA =
      if(dna.size < maxShapes) dna :+ makeOne else randomMut(dna)

    def randomMut(dna: DNA): DNA =
      dna.map(c => if(rand.nextDouble() < mutFreq) c.mutated else c).toVector

    def randomShuffle(dna: DNA): DNA = if(dna.size > 1) {
      val idx = rand.nextInt(dna.size)
      val shp = dna(idx)
      dna.patch(idx,Vector(),1).patch(rand.nextInt(dna.size-1),Vector(shp),0)
    } else dna

    def mutate(dna: DNA): DNA = {
      val ops: List[(Double, DNA => DNA)] =
        List(delFreq -> randomDel, addFreq -> randomAdd,
          mutFreq -> randomMut, shuffleFreq -> randomShuffle)
      val opsOpts = ops.map {
        case (freq, f) => if (rand.nextDouble() < freq) Some(f) else None
      }
      var result = dna
      var same = true
      opsOpts.foreach(fOpt => fOpt.foreach{f => result = f(result); same = false})
      if(!same) result else randomMut(result)
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
        val (rgbImg, rgbDna) = (img.getRGB(x, y), dnaImg.getRGB(x, y))
        val (cImg, cDna) = (new Color(rgbImg), new Color(rgbDna))
        val dRed = cImg.getRed - cDna.getRed
        val dGreen = cImg.getGreen - cDna.getGreen
        val dBlue = cImg.getBlue - cDna.getBlue
        total += dRed * dRed + dGreen * dGreen + dBlue * dBlue
      }
      total
    }
  }

  var parent = DNA.make
  val out = new PrintStream(plotfile)
  val pImg = DNA.draw(parent)
  var parentFitness = DNA.fitness(pImg)
  out.println(s"000000 ${DNA.fitness(pImg)}")
  ImageIO.write(pImg, "png", new File(s"$outdir/000000.png"))
  var lastImgDump = 0

  for(iter <- 1 to iterations) {
    val strIter = "%06d".format(iter)

    val imagesWithFitness = (1 to 4).par.map { _ =>
      val child = DNA.mutate(parent)
      val image = DNA.draw(child)
      val fitness = DNA.fitness(image)
      (child, image, fitness)
    }.toList

    val (bestChild, bestChildImg, bestChildFitness) = imagesWithFitness.sortBy(_._3).head

    if(bestChildFitness < parentFitness) {
      parentFitness = bestChildFitness
      println(s"$strIter $parentFitness ${parent.size}")
      out.println(s"$strIter $parentFitness ${parent.size}")
      parent = bestChild

      if(iter < 1000 || iter / 100 > lastImgDump) {
        ImageIO.write(bestChildImg, "png", new File(s"$outdir/$strIter.png"))
        lastImgDump = iter / 100
      }
    }
  }
}