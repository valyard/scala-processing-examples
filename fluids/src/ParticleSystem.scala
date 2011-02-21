import scala.util.Random
import java.nio.FloatBuffer
import com.sun.opengl.util._
import processing.opengl._
import javax.media.opengl._
import msafluid._

object ParticleSystem {

  var renderUsingVA = true
  val maxParticles = 5000

}

class ParticleSystem(fluidSolver:MSAFluidSolver2D) {

  var posArray:FloatBuffer = BufferUtil.newFloatBuffer( ParticleSystem.maxParticles * 2 * 2 )
  var colArray:FloatBuffer = BufferUtil.newFloatBuffer( ParticleSystem.maxParticles * 3 * 2 )
  var curIndex:Int = 0
  val particles = new Array[Particle](ParticleSystem.maxParticles)

  for ( i <- 0 to ParticleSystem.maxParticles-1 ) particles(i) = new Particle(fluidSolver)

  def updateAndDraw(width:Float, invWidth:Float, height:Float, invHeight:Float, pgl:PGraphicsOpenGL, drawFluid:Boolean) = {
    val gl = pgl.beginGL
    gl.glEnable( GL.GL_BLEND )
    if ( !drawFluid ) fadeToColor(width, height, gl, 0, 0, 0, 0.05f)

    gl.glBlendFunc( GL.GL_ONE, GL.GL_ONE )
    gl.glEnable( GL.GL_LINE_SMOOTH )
    gl.glLineWidth( 1 )

    if ( ParticleSystem.renderUsingVA ) {
      for ( i <- 0 to ParticleSystem.maxParticles-1 ) {
        if ( particles(i).alpha > 0 ) {
          particles(i).update( width, invWidth, height, invHeight )
          particles(i).updateVertexArrays( i, posArray, colArray )
        }
      }
      gl.glEnableClientState( GL.GL_VERTEX_ARRAY )
      gl.glVertexPointer( 2, GL.GL_FLOAT, 0, posArray )
      gl.glEnableClientState( GL.GL_COLOR_ARRAY )
      gl.glColorPointer( 3, GL.GL_FLOAT, 0, colArray )

      gl.glDrawArrays( GL.GL_LINES, 0, ParticleSystem.maxParticles * 2 )
    } else {
      gl.glBegin( GL.GL_LINES );
      for( i <- 0 to ParticleSystem.maxParticles ) {
        if( particles(i).alpha > 0 ) {
          particles(i).update( width, invWidth, height, invHeight )
          particles(i).drawOldSchool( gl )
        }
      }
      gl.glEnd()
    }

    gl.glDisable( GL.GL_BLEND )
    pgl.endGL()
  }

  def addParticles(x:Float, y:Float, count:Int) = {
    for ( i <- 0 to count-1 ) addParticle( x + Random.nextInt(30) - 15, y + Random.nextInt(30) - 15 )
  }

  def addParticle(x:Float, y:Float) = {
    particles(curIndex).init( x, y )
    curIndex += 1;
    if ( curIndex >= ParticleSystem.maxParticles ) curIndex = 0
  }

  def fadeToColor(width:Float, height:Float, gl:GL, r:Float, g:Float, b:Float, speed:Float) {
    gl.glBlendFunc( GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA )
    gl.glColor4f( r, g, b, speed )
    gl.glBegin( GL.GL_QUADS )
    gl.glVertex2f( 0, 0 )
    gl.glVertex2f( width, 0 )
    gl.glVertex2f( width, height )
    gl.glVertex2f( 0, height )
    gl.glEnd()
  }

}