//import com.sun.xml.internal.ws.handler.HandlerProcessor.Direction
import scala.util.Random
class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}

class ControlFunction {
  def respond(input: String): String = {
    val (opcode, paramMap) = CommandParser(input)

    if( opcode == "React" ) {
      var direction = if(Bot.lastDirection != XY.Zero) Bot.lastDirection else XY.Up
      val analyzer =  new ViewAnalyzer(new View(paramMap("view")))
      val wallsAround = analyzer.isWallAround

      if(analyzer.isWallInDirection(direction)) {
        val possibleDirections = wallsAround.filter({ case (x,y) => !y }).keys.toArray
        val randElement = Random.nextInt(possibleDirections.size)
        direction = possibleDirections(randElement)
      }
      Bot.lastDirection = direction
      "Move(direction=" + direction.toString + ")|Status(text=No Wall in Sight!)"

    } else "Move(direction=1:0)"
  }
}

// Bot State
object Bot {
  var lastDirection = XY.Zero
  var wallInDirection = false
}

object Rules {
  val Wall = 'W'
}

// Class for analyzing anything view related
class  ViewAnalyzer(view: View) {
  def isWallInDirection(direction: XY) : Boolean = view.cellAtRelPos(direction) == Rules.Wall
  def isWallAround : Map[XY,Boolean] = XY.directions.map { d => d -> isWallInDirection(d) }.toMap
}