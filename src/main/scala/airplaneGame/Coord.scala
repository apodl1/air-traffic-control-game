package airplaneGame

import scala.math

class Coord(val x: Int, val y: Int):

  override def toString: String = s"(${x}, ${y})"

  def toGridPos(coordPerTile: Int): GridPos =
    GridPos(x / coordPerTile, y / coordPerTile)

  def bearingTo(target: Coord): Int =
    if x < target.x && y < target.y then
      180 - math.atan(math.abs(x - target.x).toDouble / math.abs(y - target.y)).toDegrees.toInt
    else if x < target.x && y > target.y then
      math.atan(math.abs(x - target.x).toDouble / (y - target.y)).toDegrees.toInt
    else if x > target.x && y > target.y then
      360 - math.atan((x - target.x).toDouble / (y - target.y)).toDegrees.toInt
    else if x > target.x && y < target.y then
      180 + math.atan((x - target.x).toDouble / math.abs(y - target.y)).toDegrees.toInt
    else if x == 0 && y > target.y then
      0
    else if x == 0 && y < target.y then
      180
    else if x < target.x && y == 0 then
      90
    else
      270


  def +(that: Coord) =
    Coord(this.x + that.x, this.y + that.y)