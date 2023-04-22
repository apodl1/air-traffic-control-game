package airplaneGame

import scala.util.Random
import scala.math

class Airplane(val game: GameState, val id: Int):

  val maxTurn = 8
  val neededRunway = 3
  val maxFuel = 600
  
  var location: Coord = Coord(0, 0)
  var bearing: Degrees = Degrees(180) //TODO replace with full
  var speed: Double = 3 //TODO adjust
  var fuel: Double = maxFuel / 3 //fuel managemen in actions  TODO adjust
  

  val origin: String = "Helsinki" //TODO replace with full
  var crashed: Boolean = false

  var action: Action = Arriving(this)

  def move() = //called by game-state on every tick
    location = location + Coord((speed * math.sin(bearing.value.toDouble.toRadians)).toInt, -(speed * math.cos(bearing.value.toDouble.toRadians)).toInt)

  def slowSpeed() =
    speed = 2

  def cruiseSpeed() =
    speed = 3
  
  def fastSpeed() =
    speed = 4
  

//TODO implimentetions