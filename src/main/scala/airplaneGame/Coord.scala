package airplaneGame

import scala.math

class Coord(val x: Int, val y: Int): //class for representing pixel coordiantes on the game map with helper functions. 1 pixel always == one Coord

  override def toString: String = s"(${x}, ${y})"

  //in which GridPos is Coord located
  def toGridPos(coordPerTile: Int): GridPos =
    GridPos(x / coordPerTile, y / coordPerTile)
  
  //used in pathfinding alorithm, finds the bearing by forming a right-angled triangle, calculating the angle near the calling Coord, and transforming the result to be in regars of North-direction 
  def bearingTo(target: Coord): Int = 
    //four main cases -> four different triangles, and four cases for handling x || y == target.x || y -situations
    if x < target.x && y < target.y then
      //the triangles formed are different in each case, so the result needs to be normalized in regards to map North (performed by "180 - angle")
      180 - math.atan(math.abs(x - target.x).toDouble / math.abs(y - target.y)).toDegrees.toInt
    else if x < target.x && y > target.y then
      math.atan(math.abs(x - target.x).toDouble / (y - target.y)).toDegrees.toInt
    else if x > target.x && y > target.y then
      360 - math.atan((x - target.x).toDouble / (y - target.y)).toDegrees.toInt
    else if x > target.x && y < target.y then
      180 + math.atan((x - target.x).toDouble / math.abs(y - target.y)).toDegrees.toInt
    else if x == target.x && y > target.y then
      0
    else if x == target.x && y < target.y then
      180
    else if x < target.x then
      90
    else
      270


  //helper for adding Coords
  def +(that: Coord) =
    Coord(this.x + that.x, this.y + that.y)