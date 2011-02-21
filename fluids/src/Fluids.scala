import processing.core._
import processing.core.PConstants._
import scala.util.Random
import msafluid._
import TUIO._
import processing.opengl._
import javax.media.opengl._

object Fluids extends PApplet {

  val FLUID_WIDTH = 120f
  val tuioCursorSpeedMult = 0.02f
  val tuioStationaryForce = 0.001f
  val tuioDoubleTapDistanceThreshold2 = 0.01f
  val tuioDoubleTapTimeThreshold = 0.2f

  private var fluids:Fluids = _

  def main(args: Array[String]) = {
    fluids = new Fluids

    val frame = new javax.swing.JFrame("Fluids")
    frame.getContentPane().add(fluids)
    fluids.init

    frame.pack
    frame.setVisible(true)
  }

  implicit def Any2TuioCursor(value:Any) = value.asInstanceOf[TuioCursor]

}

class Fluids extends PApplet {

  import Fluids._

  var invWidth, invHeight, aspectRatio, aspectRatio2: Float = _
  var fluidSolver:MSAFluidSolver2D = _
  var particleSystem:ParticleSystem = _
  var imgFluid:PImage = _
  var drawFluid:Boolean = true

  var tuioClient:TuioProcessing = _
  var tuioLastTap:TuioPoint = _
  var tuioDoubleTap:Boolean = false

  override def setup() = {
    size(800, 600, OPENGL )
    hint( ENABLE_OPENGL_4X_SMOOTH )

    invWidth = 1.0f/width
    invHeight = 1.0f/height
    aspectRatio = width * invHeight
    aspectRatio2 = aspectRatio * aspectRatio

    fluidSolver = new MSAFluidSolver2D( FLUID_WIDTH.toInt, (FLUID_WIDTH * height/width).toInt )
    fluidSolver.enableRGB( true ).setFadeSpeed( 0.003f ).setDeltaT( 0.5f ).setVisc( 0.0001f )

    imgFluid = createImage( fluidSolver.getWidth, fluidSolver.getHeight, RGB )

    particleSystem = new ParticleSystem( fluidSolver )

    initTUIO
  }

  override def draw() = {
    updateTUIO
    fluidSolver.update

    if( drawFluid ) {
        for( i <- 0 to fluidSolver.getNumCells-1 ) {
            val d = 2
            imgFluid.pixels(i) = color(fluidSolver.r(i) * d, fluidSolver.g(i) * d, fluidSolver.b(i) * d)
        }
        imgFluid.updatePixels
        image(imgFluid, 0, 0, width, height)
    }

    particleSystem.updateAndDraw(width, invWidth, height, invHeight, g.asInstanceOf[PGraphicsOpenGL], drawFluid)
//    println( frameRate.toString )
  }

  override def mouseMoved = {
    val mouseNormX = mouseX * invWidth
    val mouseNormY = mouseY * invHeight
    val mouseVelX = (mouseX - pmouseX) * invWidth
    val mouseVelY = (mouseY - pmouseY) * invHeight

    addForce( mouseNormX, mouseNormY, mouseVelX, mouseVelY )
  }

  override def mousePressed = {
    drawFluid ^= true
  }

  override def keyPressed = {
    key match {
      case 'r' => {
        ParticleSystem.renderUsingVA ^= true;
        println("renderUsingVA: " + ParticleSystem.renderUsingVA)
      }
      case _ =>
    }
  }

  def addForce(x:Float, y:Float, dx:Float, dy:Float) = {
    val speed = dx * dx  + dy * dy * aspectRatio2

    if( speed > 0 ) {
      var xx = x
      var yy = y

      if ( xx < 0 ) {
        xx = 0
      } else if ( xx > 1 ) {
        xx = 1
      }

      if ( yy < 0 ) {
        var yy = 0
      } else if ( yy > 1 ) {
        yy = 1
      }

      val colorMult = 5.0f
      val velocityMult = 30.0f

      val index = fluidSolver.getIndexForNormalizedPosition( xx, yy )

      colorMode( HSB, 360, 1, 1 )
      val hue = ((xx + yy) * 180 + frameCount) % 360
      val drawColor = color( hue, 1, 1 )
      colorMode( RGB, 1 )

      fluidSolver.rOld(index)  += red(drawColor) * colorMult
      fluidSolver.gOld(index)  += green(drawColor) * colorMult
      fluidSolver.bOld(index)  += blue(drawColor) * colorMult

      particleSystem.addParticles(xx * width, yy * height, 10)
      fluidSolver.uOld(index) += dx * velocityMult
      fluidSolver.vOld(index) += dy * velocityMult
    }
  }

  def initTUIO = {
    tuioClient = new TuioProcessing( this, 3333 )
  }

  def updateTUIO = {
    val tuioCursorList = tuioClient.getTuioCursors
    for( i <- 0 to tuioCursorList.size-1 ) {
      val tcur = tuioCursorList.elementAt(i)
      var vx = tcur.getXSpeed() * tuioCursorSpeedMult
      var vy = tcur.getYSpeed() * tuioCursorSpeedMult
      if ( vx == 0 && vy == 0 ) {
        vx = Random.nextFloat * 2 * tuioStationaryForce - tuioStationaryForce
        vy = Random.nextFloat * 2 * tuioStationaryForce - tuioStationaryForce
      }
      addForce( tcur.getX, tcur.getY, vx, vy )
    }

    if ( tuioDoubleTap ) {
      drawFluid ^= true
      tuioDoubleTap = false
    }
  }

  def addTuioCursor(tcur:TuioCursor) = {
    if(tuioLastTap != null) {
      val timeMult = 0.000001f
      val nowTime = tcur.getTuioTime.getTotalMilliseconds * timeMult
      val lastTime = tuioLastTap.getTuioTime.getTotalMilliseconds * timeMult
      if( nowTime - lastTime < tuioDoubleTapTimeThreshold ) {
        val dx = tcur.getX - tuioLastTap.getX
        val dy = tcur.getY - tuioLastTap.getY
        val d2 = dx * dx + dy * dy
        if( d2 < tuioDoubleTapDistanceThreshold2 ) tuioDoubleTap = true
      }
    }
    tuioLastTap = new TuioPoint(tcur)
  }

  def addTuioObject(tobj:TuioObject) = {}
  def updateTuioObject(tobj:TuioObject) = {}
  def removeTuioObject(tobj:TuioObject) = {}
  def updateTuioCursor(tcur:TuioCursor) = {}
  def removeTuioCursor(tcur:TuioCursor) = {}
  def refresh(bundleTime:TuioTime) = {}

}

