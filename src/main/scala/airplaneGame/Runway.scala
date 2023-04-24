package airplaneGame

import airplaneGame.CompassDir.*
import scala.collection.mutable.{Queue, ArrayBuffer}

class Runway(val index: Int, val start: GridPos, val end: GridPos):

  val direction: CompassDir =
    if start.x > end.x then
      West
    else if start.x < end.x then
      East
    else if start.y < end.y then
      South
    else
      North
      
  val parallelDirection =
    if start.x > end.x then
      North
    else if start.x < end.x then
      North
    else if start.y < end.y then
      East
    else
      East
      
  val length =
    math.abs(start.x - end.x) + math.abs(start.y - end.y)
    
  val arrivingWaitArea: GridPos =
    if direction == East || direction == West then
      end + North
    else
      end + East
  
  val departingWaitArea: GridPos =
    if direction == East || direction == West then
      start + North
    else
      start + East
      
  val airplanesWaitingForGate = Queue.empty[Airplane]
  val airplanesWaitingForTakeoff = ArrayBuffer.empty[Airplane] //write about