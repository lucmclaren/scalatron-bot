//import com.sun.xml.internal.ws.handler.HandlerProcessor.Direction
import scala.util.Random
class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}

class ControlFunction {
  def respond(input: String): String = {
    val (opcode, paramMap) = CommandParser(input)

    if( opcode == "React" ) {
      Bot.behave(paramMap)
    } else ""
  }
}


}

object Bot {
  def behave(paramMap: Map[String, String]): String = {
    try {
      var direction = XY(paramMap.getOrElse("masterLastDirection", XY.Up.toString))
      val view = new View(paramMap("view"))
      val analyzer = new ViewAnalyzer(view)
      val dirGoodObj, offsetGoodObj = dirToGoodObj(view)

      // TODO: change direction to next goodObj, if a badObj is around. Not Random!
      val badObjAround = analyzer.isBadObjAround
      if (analyzer.isBadObjInDirection(direction)) {
        val possibleDirections = badObjAround.filter({ case (x, y) => !y }).keys.toArray
        val randElement = Random.nextInt(possibleDirections.size)
        direction = possibleDirections(randElement)
      }

      s"Move(direction=$direction)|Set(masterLastDirection=$direction)"
    }
    catch {
      case e: Exception => "Log(text=" + e.printStackTrace + ")"
    }
  }

  def dirToGoodObj(view: View): (XY, Int) = {
    val offsetFluppet = view.offsetToNearest(Rules.Fluppet)
    val offsetZugar = view.offsetToNearest(Rules.Zugar)

    (offsetFluppet, offsetZugar) match {
      case (Some(f), Some(z)) if (z.stepsTo(view.center) >= f.stepsTo(view.center)) => (f.signum, f.stepsTo(view.center))
      case (_, Some(z)) => (z.signum, z.stepsTo(view.center))
      case (Some(f), None) => (f.signum, f.stepsTo(view.center))
      case (None, None) => (XY.Zero, 0)
    }
  }
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