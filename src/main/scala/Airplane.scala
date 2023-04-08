import scala.util.Random

class Airplane(game: GameState, id: Int, windowSize: (Int, Int)):

  var location: Coord = Coord(Random.nextInt(windowSize._1), Random.nextInt(windowSize._2))
  var bearing: Int = 90 //TODO replace with full
  var speed: Int = 10 //TODO adjust
  var fuel: Double = 180 //TODO adjust

  val origin: String = "Helsinki" //TODO replace with full
  var crashed: Boolean = false

  var action: Action = ???
 
  def slowSpeed() =
    speed = 5

  def cruiseSpeed() =
    speed = 10
  
  def fastSpeed() =
    speed = 15
  

//TODO implimentetions