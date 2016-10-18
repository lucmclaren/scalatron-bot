//import com.sun.xml.internal.ws.handler.HandlerProcessor.Direction
import scala.util.Random
class ControlFunctionFactory {
  def create = try { new ControlFunction().respond _ } catch { case e: Throwable => e.printStackTrace(); "" }
}

class ControlFunction {
  def respond(input: String): String = {
    val (opcode, paramMap) = CommandParser(input)

    if( opcode == "React" ) {
      try {
        var direction = if (Bot.lastDirection != XY.Zero) Bot.lastDirection else XY.Up
        val view = new View(paramMap("view"))
        val analyzer = new ViewAnalyzer(view)

        val offsetFluppet = view.offsetToNearest(Rules.Fluppet)
        val offsetZugar = view.offsetToNearest(Rules.Zugar)

        (offsetFluppet, offsetZugar) match {
          case (Some(f), Some(z)) if (z.stepsTo(view.center) >= f.stepsTo(view.center)) => direction = f.signum
          case (_, Some(z))     => direction = z.signum
          case (Some(f), None)  => direction = f.signum
          case (None, None)     => direction = Bot.lastDirection
        }

        val badObjAround = analyzer.isBadObjAround
        if (analyzer.isBadObjInDirection(direction)) {
          val possibleDirections = badObjAround.filter({ case (x, y) => !y }).keys.toArray
          val randElement = Random.nextInt(possibleDirections.size)
          direction = possibleDirections(randElement)
        }

        Bot.lastDirection = direction

        "Move(direction=" + direction.toString + ")"
      }
      catch {
        case e: Exception => "Log(text=" + e.printStackTrace + ")"
      }
    } else ""
  }
}

// Bot State
object Bot {
  var lastDirection   = XY.Zero
  var wallInDirection = false
}

object Rules {
  val Wall      = 'W'
  val Master    = 'M'
  val Enemy     = 'm'
  val Zugar     = 'P'
  val Toxifera  = 'p'
  val Fluppet   = 'B'
  val Snorg     = 'b'
  val Slave     = 'S'
  val Minibot   = 's'

  val BadObj = List(Wall, Enemy, Toxifera, Snorg, Minibot)
}

// Class for analyzing anything view related
class  ViewAnalyzer(view: View) {
  def isInDirection(direction: XY, rule: Char) : Boolean = view.cellAtRelPos(direction) == rule
  def isAround(rule: Char) : Map[XY,Boolean] = XY.directions.map { d => d -> isInDirection(d, rule) }.toMap

  def isBadObjInDirection(direction: XY) : Boolean = Rules.BadObj.contains(view.cellAtRelPos(direction))
  def isBadObjAround : Map[XY, Boolean] = XY.directions.map { d => d ->  isBadObjInDirection(d)}.toMap
}