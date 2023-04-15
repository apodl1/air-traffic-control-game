package airplaneGame

import scala.math

class Coord(val x: Int, val y: Int):

  def toGridPos(coordPerTile: Int): GridPos =
    GridPos(x / coordPerTile, y / coordPerTile)

  def +(that: Coord) =
    Coord(this.x + that.x, this.y + that.y)