package airplaneGame

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Queue}

class GameState(val width: Int, val height: Int, val bufferSize: Int, val coordPerTile: Int):  //placeholder class
  
  val grid = Grid(width, height, bufferSize, coordPerTile)
  var planeIndexes = 0
  val airplanesOnMap = ArrayBuffer.empty[Airplane]
  val airplanesToArrive = ArrayBuffer.empty[Airplane]
  val crashedPlanes = Queue.empty[Airplane]

  var clock = 0

  var sinceNewAirplane = 0

  def tick(): Unit =
    clock += 1
    sinceNewAirplane += 1

    (airplanesToArrive ++ airplanesOnMap ++ crashedPlanes).foreach( _.action.execute() )
    airplanesOnMap.foreach( _.move() )

    if sinceNewAirplane == 10 && airplanesOnMap.length < 2 then
      planeIndexes += 1
      airplanesToArrive.append(Airplane(this, planeIndexes))
      sinceNewAirplane = 0
  end tick


  def planeAtCoord(coord: Coord): Option[Airplane] =
    airplanesOnMap.find(n => math.abs(n.location.x - coord.x) < 30 && math.abs(n.location.y - coord.y) < 30)

  def runwayAtCoord(coord: Coord): Option[Runway] =
    grid.runways.find( _.start.isSameAs(coord.toGridPos(coordPerTile)) )
