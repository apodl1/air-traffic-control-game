package airplaneGame

import scala.collection.mutable.{ArrayBuffer, Queue}
import scala.util.Random

class GameState(val width: Int, val height: Int, val bufferSize: Int, val coordPerTile: Int):  //placeholder class
  
  val grid = Grid(width, height, bufferSize, coordPerTile)
  private var planeIndexes = 0
  val airplanesOnMap = ArrayBuffer.empty[Airplane]
  val airplanesToArrive = ArrayBuffer.empty[Airplane]
  val crashedPlanes = Queue.empty[Airplane]
  val arrivingMessages = Queue.empty[String]
  var score = 0
  var latestScoreMessage = ""

  def newScore(message: String, points: Int) =
    score += points
    latestScoreMessage = message + "\nPoints awarded: " + points + "!\n"

  def newArrivalMessage(message: String) =
    arrivingMessages.enqueue(message)
    if arrivingMessages.length > 3 then
      arrivingMessages.dequeue()

  private var clock = 0

  private var sinceNewAirplane = 0

  def tick(): Unit =
    clock += 1
    sinceNewAirplane += 1

    (airplanesToArrive ++ airplanesOnMap ++ crashedPlanes).foreach( _.action.execute() )
    (airplanesOnMap ++ crashedPlanes).foreach( _.move() )
    airplanesOnMap.filter( _.fuel < 0 ).foreach( n => n.action = Crashed(n) )

    if sinceNewAirplane == 10 && airplanesOnMap.length < 2 then
      planeIndexes += 1
      val plane =
        val random = Random.nextInt(3)
        if random == 0 then
          SmallPlane(this, planeIndexes)
        if random == 1 then
          MediumPlane(this, planeIndexes)
        else
          BigPlane(this, planeIndexes)
      airplanesToArrive.append(plane)
      newArrivalMessage(ArrivingPlaneMessage(plane).message)
      sinceNewAirplane = 0
  end tick


  def planeAtCoord(coord: Coord): Option[Airplane] =
    airplanesOnMap.find(n => math.abs(n.location.x - coord.x) < 30 && math.abs(n.location.y - coord.y) < 30)

  def runwayAtCoord(coord: Coord): Option[Runway] =
    grid.runways.find( _.start.isSameAs(coord.toGridPos(coordPerTile)) )
