package airplaneGame

import scala.collection.mutable.Buffer

//class for representing larger squares use for map generation, the ration of pixels (Coords) to GridPos is passed as parameter to GameState and can vary if needed
class GridPos(val x: Int, val y: Int):

  override def toString: String = s"${x}, ${y}"

  //helpers for adding GridPos and/or CompassDir
  def +(that: GridPos): GridPos = GridPos(this.x + that.x, this.y + that.y)
  def -(that: GridPos): GridPos = GridPos(this.x - that.x, this.y - that.y)

  //one CompassDir -> one GridPos in that direction
  def +(that: CompassDir): GridPos = GridPos(this.x + that.xStep, this.y + that.yStep)
  def -(that: CompassDir): GridPos = GridPos(this.x - that.xStep, this.y - that.yStep)

  //helper for comparing GridPos:es
  def isSameAs(that: GridPos): Boolean =
    this.x == that.x && this.y == that.y
  
  //convert GridPos to Coord, return coord in the middle of Grid Pos, needs the ratio on GridPos to Coord as parameter
  def toCoord(coordPerTile: Int): Coord =
    Coord(x * coordPerTile + coordPerTile / 2, y * coordPerTile + coordPerTile / 2)