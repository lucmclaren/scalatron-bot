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
        val possibleDirections = wallsAround.filter({ case (x,y) => y != true }).keys.toArray
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

// Class for analyzing anything view related
class  ViewAnalyzer(view: View) {
  def isWallInDirection(direction: XY) : Boolean = {
    val nextFieldInDirection = direction match {
      case XY.Left      => view.cellAtRelPos(XY.Left)
      case XY.LeftDown  => view.cellAtRelPos(XY.LeftDown)
      case XY.Down      => view.cellAtRelPos(XY.Down)
      case XY.DownRight => view.cellAtRelPos(XY.DownRight)
      case XY.Right     => view.cellAtRelPos(XY.Right)
      case XY.RightUp   => view.cellAtRelPos(XY.RightUp)
      case XY.Up        => view.cellAtRelPos(XY.Up)
      case XY.UpLeft    => view.cellAtRelPos(XY.UpLeft)
    }
    if(nextFieldInDirection == 'W') {
      true
    } else
      false
  }
  def isWallAround : Map[XY,Boolean] = {
    Map(
      XY.Left -> isWallInDirection(XY.Left),
      XY.LeftDown -> isWallInDirection(XY.LeftDown),
      XY.Down -> isWallInDirection(XY.Down),
      XY.DownRight -> isWallInDirection(XY.DownRight),
      XY.Right -> isWallInDirection(XY.Right),
      XY.RightUp -> isWallInDirection(XY.RightUp),
      XY.Up -> isWallInDirection(XY.Up),
      XY.UpLeft -> isWallInDirection(XY.UpLeft)
    )
  }
}