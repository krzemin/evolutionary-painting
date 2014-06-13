import java.awt.image.BufferedImage
import java.awt.Color
import java.io.{PrintStream, File}
import javax.imageio.ImageIO
import scala.util.Random

object EvolutionaryPainting extends App {

  val rand = new Random()

  val iterations = 1000000
  val rRatio = 0.04
  val initCircles = 5
  val addFreq = 0.22
  val delFreq = 0.21
  val mutFreq = 0.02
  val mutCoordRatio = 0.33
  val mutRRatio = 0.45
  val mutColorRatio = 0.15
  val mutAlphaRatio = 0.1
  val minAlpha = 60
  val maxAlpha = 180
  val maxCircles = 5000

  val img: BufferedImage = ImageIO.read(new File("images/rynek2.png"))
  val (w, h) = (img.getWidth, img.getHeight)
  val diagonal = math.sqrt(w*w + h*h).toInt / 4 * 3

  case class Oval(x: Int, y: Int, r1: Int, r2: Int, c: Color)

  object Oval {
    def apply(): Oval = {
      val r1 = 1 + rand.nextInt((diagonal * rRatio).toInt)
      val r2 = 1 + rand.nextInt((diagonal * rRatio).toInt)
      val x = rand.nextInt(w - r1)
      val y = rand.nextInt(h - r2)
      val c = new Color(
        rand.nextInt(256),
        rand.nextInt(256),
        rand.nextInt(256),
        minAlpha + rand.nextInt(maxAlpha - minAlpha)
      )
      Oval(x, y, r1, r2, c)
    }
    def mutate(circle: Oval): Oval = {
      val xMut = circle.x + ((rand.nextGaussian() - 0.5) * circle.r2 * mutCoordRatio).toInt
      val yMut = circle.y + ((rand.nextGaussian() - 0.5) * circle.r1 * mutCoordRatio).toInt
      val r1Mut = circle.r1 //+ ((rand.nextGaussian() - 0.5) * circle.r * mutRRatio).toInt
      val r2Mut = circle.r2
      val cMut = new Color(
        (circle.c.getRed + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (circle.c.getGreen + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (circle.c.getBlue + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (circle.c.getAlpha + ((rand.nextGaussian() - 0.5) * (maxAlpha - minAlpha) * mutAlphaRatio).toInt).max(minAlpha).min(maxAlpha)
      )
      circle.copy(
        xMut.max(0).min(w - r1Mut),
        yMut.max(0).min(h - r2Mut),
        r1Mut.max(1),
        r2Mut.max(1),
        cMut)
    }
  }

  type DNA = Vector[Oval]

  object DNA {
    def make: DNA = Vector.fill(initCircles)(Oval())
    def randomDel(dna: DNA): DNA = dna.patch(rand.nextInt(dna.size), Nil, 1)
    def randomAdd(dna: DNA): DNA =
      Oval() +: (if(dna.size < maxCircles) dna else randomDel(dna))
    def randomMut(dna: DNA): DNA =
      dna.map(c => if(rand.nextDouble() < mutFreq) Oval.mutate(c) else c).toVector

    def draw(dna: DNA): BufferedImage = {
      val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
      val g = image.getGraphics
      g.setColor(Color.BLACK)
      g.fillRect(0, 0, w, h)
      dna.foreach { circle =>
        g.setColor(circle.c)
        g.fillOval(circle.x, circle.y, circle.r1, circle.r2)
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

  for(iter <- 1 to iterations) {
    val strIter = "%06d".format(iter)

    val funs: List[DNA => DNA] = List(DNA.randomAdd, DNA.randomDel, DNA.randomAdd, DNA.randomMut)
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
      ImageIO.write(bestChildImg, "png", new File(s"images/$strIter.png"))
    }
  }

  println("finished")
}