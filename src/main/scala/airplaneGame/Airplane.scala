package airplaneGame

import scala.util.Random
import scala.math

class Airplane(val game: GameState, id: Int, coordSize: (Int, Int)):

  val maxTurn = 2
  val neededRunway = 5
  val maxFuel = 600
  
  var location: Coord = Coord(Random.nextInt(coordSize._1), Random.nextInt(coordSize._2))
  var bearing: Int = 90 //TODO replace with full
  var speed: Double = 10 //TODO adjust
  var fuel: Double = maxFuel / 3 //fuel managemen in actions  TODO adjust
  

  val origin: String = "Helsinki" //TODO replace with full
  var crashed: Boolean = false

  var action: Action = ???

  def move() =
    location = location + Coord((speed * math.sin(bearing.toDouble.toRadians)).toInt, (speed * math.cos(bearing.toDouble.toRadians)).toInt)

  def slowSpeed() =
    speed = 5

  def cruiseSpeed() =
    speed = 10
  
  def fastSpeed() =
    speed = 15
  

//TODO implimentetions