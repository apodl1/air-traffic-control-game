package airplaneGame

import scala.util.Random
import scala.math

trait Airplane(val game: GameState, val id: Int):

  val model: String //implemented by classes of different plane types
  val maxTurn: Int
  val neededRunway: Int
  val startFuel: Double
  var fuel: Double //updated by Action-classes of flying planes
  val origin: String
  var passengers: Int
  val fuelConsumption = 0.2
  val maxSpeed = 4.0

  var location: Coord = Coord(0, 0) //updated in Action-classes
  var bearing: Degrees = Degrees(180)
  var speed: Double = 3

  def fuelToDisplay = fuel.floor //for GUI showing

  var action: Action = Arriving(this) //modified by Action-classes themselves and by user through GUI-buttons located in AirplaneGame

  def move() = //called by game-state on every tick for every plane
    location = location + Coord((speed * math.sin(bearing.value.toDouble.toRadians)).toInt, -(speed * math.cos(bearing.value.toDouble.toRadians)).toInt)

  //called by GUI buttons
  def slowSpeed() =
    speed = 2
  def cruiseSpeed() =
    speed = 3
  def fastSpeed() =
    speed = 4



class SmallPlane(game: GameState, id: Int) extends Airplane(game, id):
  val model = "Airbus 330"
  val maxTurn = 14
  val neededRunway = 3
  val startFuel = 220.0
  var fuel: Double =  startFuel + Random.nextInt(50)
  val origin = smallOrigins(Random.nextInt(smallOrigins.length)) //located in displayedTexts
  var passengers = Random.nextInt(140) + 30


class MediumPlane(game: GameState, id: Int) extends Airplane(game, id):
  val model = "Boeing 737-800"
  val maxTurn = 12
  val neededRunway = 4
  val startFuel = 280.0
  var fuel: Double =  startFuel + Random.nextInt(50)
  val origin = (smallOrigins ++ smallOrigins ++ bigOrigins)(Random.nextInt(smallOrigins.length * 2 + bigOrigins.length))
  var passengers = Random.nextInt(120) + 100


class BigPlane(game: GameState, id: Int) extends Airplane(game, id):
  val model = "Boeing 777X"
  val maxTurn = 10
  val neededRunway = 6
  val startFuel = 300.0
  var fuel: Double =  startFuel + Random.nextInt(100)
  val origin = bigOrigins(Random.nextInt(bigOrigins.length))
  var passengers = Random.nextInt(170) + 250