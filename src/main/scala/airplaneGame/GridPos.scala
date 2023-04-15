package airplaneGame

import scala.collection.mutable.Buffer

class GridPos(val x: Int, val y: Int):

  override def toString: String = s"${x}, ${y}" 

  def neighborOption(direction: CompassDir, gridSize: (Int, Int)): Option[GridPos] =
    val neighborPos = GridPos(x + direction.xStep, y + direction.yStep)
    if neighborPos.x > (gridSize._1 - 1) || neighborPos.x < 0 || neighborPos.y > (gridSize._2 - 1) || neighborPos.y < 0
      then None
    else
      Some(neighborPos)

  def neighbors(gridSize: (Int, Int)): Array[GridPos] =
    val res = Buffer.empty[Option[GridPos]]
    res.append(neighborOption(CompassDir.North, gridSize))
    res.append(neighborOption(CompassDir.East, gridSize))
    res.append(neighborOption(CompassDir.South, gridSize))
    res.append(neighborOption(CompassDir.West, gridSize))
    res.flatten.toArray

  def +(that: GridPos): GridPos = GridPos(this.x + that.x, this.y + that.y)
  def -(that: GridPos): GridPos = GridPos(this.x - that.x, this.y - that.y)
  
  def +(that: CompassDir): GridPos = GridPos(this.x + that.xStep, this.y + that.yStep)
  def -(that: CompassDir): GridPos = GridPos(this.x - that.xStep, this.y - that.yStep)
  
  def toCoord(coordPerTile: Int): Coord =
    Coord(x * coordPerTile + coordPerTile / 2, y * coordPerTile + coordPerTile / 2)