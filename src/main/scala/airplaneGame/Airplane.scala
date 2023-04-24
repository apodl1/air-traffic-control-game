package airplaneGame

import scala.util.Random
import scala.math

trait Airplane(val game: GameState, val id: Int):

  val model: String
  val maxTurn: Int
  val neededRunway: Int
  val maxFuel: Double
  var fuel: Double
  val origin: String
  val passengers: Int
  
  var location: Coord = Coord(0, 0)
  var bearing: Degrees = Degrees(180)
  var speed: Double = 3 //TODO adjust

  def fuelToDisplay = fuel.floor

  var action: Action = Arriving(this)

  def move() = //called by game-state on every tick
    location = location + Coord((speed * math.sin(bearing.value.toDouble.toRadians)).toInt, -(speed * math.cos(bearing.value.toDouble.toRadians)).toInt)

  def slowSpeed() =
    speed = 2

  def cruiseSpeed() =
    speed = 3
  
  def fastSpeed() =
    speed = 4



class SmallPlane(game: GameState, id: Int) extends Airplane(game, id):
  val model = "Airbus 330"
  val maxTurn = 10
  val neededRunway = 3
  val maxFuel = 250.0
  var fuel: Double =  maxFuel //fuel managemen in actions
  val origin = smallOrigins(Random.nextInt(smallOrigins.length))
  val passengers = Random.nextInt(140) + 30


class MediumPlane(game: GameState, id: Int) extends Airplane(game, id):
  val model = "Boeing 737-800"
  val maxTurn = 10
  val neededRunway = 4
  val maxFuel = 300.0
  var fuel: Double =  maxFuel //fuel managemen in actions
  val origin = (smallOrigins ++ bigOrigins)(Random.nextInt(smallOrigins.length + bigOrigins.length))
  val passengers = Random.nextInt(120) + 100


class BigPlane(game: GameState, id: Int) extends Airplane(game, id):
  val model = "Boeing 777X"
  val maxTurn = 8
  val neededRunway = 6
  val maxFuel = 400.0
  var fuel: Double =  maxFuel //fuel managemen in actions
  val origin = bigOrigins(Random.nextInt(bigOrigins.length))
  val passengers = Random.nextInt(170) + 250