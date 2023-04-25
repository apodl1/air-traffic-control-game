package airplaneGame

import airplaneGame.CompassDir.*
import scala.collection.mutable.{Queue, ArrayBuffer}

//class representing a runway on the map, contains its index, start and end point and other derived information
class Runway(val index: Int, val start: GridPos, val end: GridPos):

  //direction of runway, computed by comparing coordiantes of start and end
  val direction: CompassDir =
    if start.x > end.x then
      West
    else if start.x < end.x then
      East
    else if start.y < end.y then
      South
    else
      North
      
  //generalized version of direction, returns true if runway is horizontal
  val isHorizontal = direction == East || direction == West
      
  //returns a direction at a 90 degree angle to runways true direction. Is North for horizontals and East for verticals 
  val parallelDirection =
    if start.x > end.x then
      North
    else if start.x < end.x then
      North
    else if start.y < end.y then
      East
    else
      East
      
  //length of runway
  val length =
    math.abs(start.x - end.x) + math.abs(start.y - end.y)
    
  //location of airplane wait areas. Same principle as parallelDirection
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
      
  //mutable collections for storing relevant airplanes. Modified by Action-classes
  val airplanesWaitingForGate = Queue.empty[Airplane]
  val airplanesWaitingForTakeoff = ArrayBuffer.empty[Airplane] //write about