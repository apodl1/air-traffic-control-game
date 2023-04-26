package airplaneGame

import scala.collection.mutable.{ArrayBuffer, Queue}
import scala.util.Random

//the central class unifying part into a game. The main interface between backend and GUI
class GameState(val width: Int, val height: Int, val bufferSize: Int, val coordPerTile: Int):  //placeholder class
  
  val grid = Grid(width, height, bufferSize, coordPerTile)
  private var planeIndexes = 0
  val airplanesOnMap = ArrayBuffer.empty[Airplane]
  val airplanesToArrive = ArrayBuffer.empty[Airplane]
  val crashedPlanes = Queue.empty[Airplane]
  val arrivingMessages = Queue.empty[String]
  //scoring managed by plane Actions
  var score = 0
  var latestScoreMessage = ""

  //for displaying score info
  def newScore(message: String, points: Int) =
    score += points
    latestScoreMessage = message + "\nPoints awarded: " + points + "!\n"

  //for displaying arrival info 
  def newArrivalMessage(message: String) =
    arrivingMessages.enqueue(message)
    if arrivingMessages.length > 6 then
      arrivingMessages.dequeue()

  private var sinceNewAirplane = 150

  //called by GUI when time advances
  def tick(): Unit =
    sinceNewAirplane += 1

    (airplanesToArrive ++ airplanesOnMap ++ crashedPlanes).foreach( _.action.execute() )
    (airplanesOnMap ++ crashedPlanes).foreach( _.move() )
    airplanesOnMap.filter( _.fuel < 0 ).foreach( n => n.action = Crashed(n) ) //crashes the plane if it is out of fuel

    //spawns new planes
    if sinceNewAirplane == 200 && airplanesOnMap.length < 18 then
      planeIndexes += 1
      val plane =
        val random = Random.nextInt(3)
        if random == 0 then
          SmallPlane(this, planeIndexes)
        else if random == 1 then
          MediumPlane(this, planeIndexes)
        else
          BigPlane(this, planeIndexes)
      airplanesToArrive.append(plane)
      newArrivalMessage(ArrivingPlaneMessage(plane).message)
      sinceNewAirplane = 0
  end tick

  //use by crash condition detection
  def planeAtCoord(coord: Coord): Option[Airplane] =
    airplanesOnMap.find(n => math.abs(n.location.x - coord.x) < 30 && math.abs(n.location.y - coord.y) < 30)

  //used by GUI for user input
  def runwayAtCoord(coord: Coord): Option[Runway] =
    grid.runways.find( _.start.isSameAs(coord.toGridPos(coordPerTile)) )
