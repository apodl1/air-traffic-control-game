package airplaneGame

import scala.util.Random
import scala.math

trait Airplane(val game: GameState, val id: Int):

  val model: String //implimented by classes of different plane types
  val maxTurn: Int
  val neededRunway: Int
  val maxFuel: Double
  var fuel: Double //update by Action-classes of flying planes
  val origin: String
  val passengers: Int
  val fuelConsumption = 0.2
  val maxSpeed = 4.0
  
  var location: Coord = Coord(0, 0) //updated in Action-classes
  var bearing: Degrees = Degrees(180)
  var speed: Double = 3

  def fuelToDisplay = fuel.floor //for GUI showing

  var action: Action = Arriving(this) //modified by Action-classes themselves and by user through GUI-buttons located in AirplaneGame

  def move() = //called by game-state on every tick for every plane
    location = location + Coord((speed * math.sin(bearing.value.toDouble.toRadians)).toInt, -(speed * math.cos(bearing.value.toDouble.toRadians)).toInt)

  def slowSpeed() = //called by GUI buttons
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
  var fuel: Double =  maxFuel
  val origin = smallOrigins(Random.nextInt(smallOrigins.length)) //located in displayedTexts
  val passengers = Random.nextInt(140) + 30


class MediumPlane(game: GameState, id: Int) extends Airplane(game, id):
  val model = "Boeing 737-800"
  val maxTurn = 10
  val neededRunway = 4
  val maxFuel = 300.0
  var fuel: Double =  maxFuel
  val origin = (smallOrigins ++ bigOrigins)(Random.nextInt(smallOrigins.length + bigOrigins.length))
  val passengers = Random.nextInt(120) + 100


class BigPlane(game: GameState, id: Int) extends Airplane(game, id):
  val model = "Boeing 777X"
  val maxTurn = 8
  val neededRunway = 6
  val maxFuel = 400.0
  var fuel: Double =  maxFuel
  val origin = bigOrigins(Random.nextInt(bigOrigins.length))
  val passengers = Random.nextInt(170) + 250