import java.awt.image.BufferedImage
import java.awt.Color
import java.io.{PrintStream, File}
import javax.imageio.ImageIO
import scala.util.Random

object EvolutionaryPainting extends App {

  val rand = new Random()

  val iterations = 1000000
  val rRatio = 0.15
  val initCircles = 5
  val addFreq = 0.22
  val delFreq = 0.21
  val mutFreq = 0.02
  val mutCoordRatio = 0.33
  val mutRRatio = 0.45
  val mutColorRatio = 0.04
  val mutAlphaRatio = 0.1
  val minAlpha = 20
  val maxAlpha = 120
  val maxCircles = 1000

  val img: BufferedImage = ImageIO.read(new File("images/rynek2.png"))
  val (w, h) = (img.getWidth, img.getHeight)
  val diagonal = math.sqrt(w*w + h*h).toInt / 4 * 3

  case class Circle(x: Int, y: Int, r: Int, c: Color)

  object Circle {
    def apply(): Circle = {
      val r = 1 + rand.nextInt((diagonal * rRatio).toInt)
      val x = rand.nextInt(w - r)
      val y = rand.nextInt(h - r)
      val c = new Color(
        rand.nextInt(256),
        rand.nextInt(256),
        rand.nextInt(256),
        minAlpha + rand.nextInt(maxAlpha - minAlpha)
      )
      Circle(x, y, r, c)
    }
    def mutate(circle: Circle): Circle = {
      val xMut = circle.x + ((rand.nextGaussian() - 0.5) * circle.r * mutCoordRatio).toInt
      val yMut = circle.y + ((rand.nextGaussian() - 0.5) * circle.r * mutCoordRatio).toInt
      val rMut = circle.r + ((rand.nextGaussian() - 0.5) * circle.r * mutRRatio).toInt
      val cMut = new Color(
        (circle.c.getRed + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (circle.c.getGreen + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (circle.c.getBlue + ((rand.nextGaussian() - 0.5) * 255 * mutColorRatio).toInt).max(0).min(255),
        (circle.c.getAlpha + ((rand.nextGaussian() - 0.5) * (maxAlpha - minAlpha) * mutAlphaRatio).toInt).max(minAlpha).min(maxAlpha)
      )
      circle.copy(
        xMut.max(0).min(w - rMut),
        yMut.max(0).min(h - rMut),
        rMut.max(1),
        cMut)
    }
  }

  type DNA = Vector[Circle]

  object DNA {
    def make: DNA = Vector.fill(initCircles)(Circle())
    def mutate(dna: DNA): DNA = {
      val dna1 =
        if(rand.nextDouble() < delFreq)
          dna.patch(rand.nextInt(dna.size), Nil, 1)
        else
          dna

      val dna2 =
        if(dna1.size < maxCircles && rand.nextDouble() < addFreq)
          Circle() +: dna1
        else
          dna1

      dna2.par.map(c => if(rand.nextDouble() < mutFreq) Circle.mutate(c) else c).toVector
    }
    def draw(dna: DNA): BufferedImage = {
      val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
      val g = image.getGraphics
      g.setColor(Color.BLACK)
      g.fillRect(0, 0, w, h)
      dna.foreach { circle =>
        g.setColor(circle.c)
        g.fillOval(circle.x, circle.y, circle.r, circle.r)
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
  out.println(s"000000 ${DNA.fitness(pImg)}")
  ImageIO.write(pImg, "png", new File(s"images/000000.png"))

  for(iter <- 1 to iterations) {
    val strIter = "%06d".format(iter)

    val child = DNA.mutate(parent)

    val List(parentImg, childImg) = List(parent, child).par.map(DNA.draw).toList
    val List(parentF, childF) = List(parentImg, childImg).par.map(DNA.fitness).toList

    if(childF < parentF) {
      out.println(s"$strIter $parentF ${parent.size}")
      parent = child
      ImageIO.write(childImg, "png", new File(s"images/$strIter.png"))
    }
  }

  println("finished")
}