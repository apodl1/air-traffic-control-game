package airplaneGame

import scala.collection.mutable.ArrayBuffer

class GameState(val width: Int, val height: Int, val bufferSize: Int, val coordPerTile: Int):  //placeholder class
  
  val grid = Grid(width, height, bufferSize, coordPerTile)
  var planeIndexes = 0
  val airplanesOnMap = ArrayBuffer.empty[Airplane]
  val airplanesToArrive = ArrayBuffer.empty[Airplane]

  var clock = 0

  var sinceNewAirplane = 0

  def tick(): Unit =
    clock += 1
    sinceNewAirplane += 1

    (airplanesToArrive ++ airplanesOnMap).foreach( _.action.execute() )
    airplanesOnMap.foreach( _.move() )

    if sinceNewAirplane == 10 then
      airplanesToArrive.append(Airplane(this, planeIndexes))
      //sinceNewAirplane = 0
