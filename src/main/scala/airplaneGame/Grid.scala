package airplaneGame

import CompassDir.{East, North, South, West}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Grid(val width: Int, val height: Int, bufferSize: Int, val coordPerTile: Int):

  val size = (width, height)
  val coordSize = (width * coordPerTile, height * coordPerTile)

  val runways = ArrayBuffer.empty[Runway]
  def numberOfRunways = runways.length
  val gates = ArrayBuffer.empty[Gate]
  def numberOfGates = gates.length

  val currentGrid: Vector[Vector[Square]] =
    LazyList.iterate(0)(n => n + 1)
      .take(height)
      .toVector
      .map( y =>
        LazyList.iterate(0)(n => n + 1)
          .take(width)
          .toVector
          .map( x => Square(this, GridPos(x, y)) ) )

  def renderable: Vector[String] =
    currentGrid.flatten.map( n => n.tile.getOrElse("grounddd") )
  
  def squareAt(gridPos: GridPos): Square =
    currentGrid(gridPos.y)(gridPos.x)

  def squareAt(x: Int, y: Int): Square =
    currentGrid(y)(x)

  def nextNSquares(startPos: GridPos, direction: CompassDir, n: Int): Vector[Square] =
    LazyList.iterate(startPos + direction)( _ + direction).take(n).map( n => squareAt(n) ).toVector


  def generate(runways: Int, terminalSize: (Int, Int), runwayLengths: Vector[Int]): Unit =
    placeTerminal(terminalSize._1, terminalSize._2)
    (1 to runways).foreach(n =>
      if Random.nextInt(2) == 0 then
        placeRunwayHorizontal(runwayLengths(Random.nextInt(runwayLengths.length)))
      else
        val possibleLengths = runwayLengths.filter( _ <= height - bufferSize * 2 )
        placeRunwayVertical(possibleLengths(Random.nextInt(possibleLengths.length))))


  def placeTerminal(terminalWidth: Int, terminalHeight: Int): Unit = //called first
    require(terminalWidth > 2 && terminalHeight > 2)
    val xPos = Random.nextInt(width - (bufferSize - 2) * 2 - terminalWidth) + bufferSize - 1
    val yPos = Random.nextInt(height - (bufferSize - 2) * 2 - terminalHeight) + bufferSize - 1
    val NW = GridPos(xPos, yPos)
    squareAt(NW).tile = Some("terminNW")
    nextNSquares(NW, East, terminalWidth - 2).foreach( n => { n.tile = Some("terminaN"); gates.append(Gate(numberOfGates, n.loc)) } )
    nextNSquares(NW, South, terminalHeight - 2).foreach( n => n.tile = Some("terminaW") )
    val NE = GridPos(xPos + terminalWidth - 1, yPos)
    squareAt(NE).tile = Some("terminNE")
    nextNSquares(NE, South, terminalHeight - 2).foreach( n => n.tile = Some("terminaE") )
    val SW = GridPos(xPos, yPos + terminalHeight - 1)
    squareAt(SW).tile = Some("terminSW")
    nextNSquares(SW, East, terminalWidth - 2).foreach( n => n.tile = Some("terminaS") )
    val SE = GridPos(xPos + terminalWidth - 1, yPos + terminalHeight - 1)
    squareAt(SE).tile = Some("terminSE")
    
  def placeRunwayHorizontal(length: Int): Unit = //calledSecond
    require(length > 2)
    val xPos = Random.nextInt(width - bufferSize * 2 - length) + bufferSize
    val yPos = Random.nextInt(height - bufferSize * 2) + bufferSize
    val edge1 = GridPos(xPos, yPos)
    //println(nextNSquares(edge1, East, length - 1).map( _.tile.getOrElse("") ).mkString(","))
    if squareAt(edge1).tile.isDefined
      || !nextNSquares(edge1, East, length + 1).forall( _.tile.isEmpty )
      || !nextNSquares(edge1, West, 2).forall( _.tile.isEmpty )
      || !nextNSquares(edge1 + North - East, East, length + 1).forall( _.tile.isEmpty )
      || !nextNSquares(edge1 + South - East, East, length + 1).forall( _.tile.isEmpty ) then
      placeRunwayHorizontal(length)
    else
      squareAt(edge1).tile = Some("runwaHSE")
      nextNSquares(edge1, East, length - 2).foreach( _.tile = Some("runwayHH") )
      val edge2 = GridPos(xPos + length - 1, yPos)
      squareAt(edge2).tile = Some("runwaHEE")
      if Random.nextInt(2) == 0 then
        runways.append(Runway(numberOfRunways, edge1, edge2))
      else
        runways.append(Runway(numberOfRunways, edge2, edge1))
      squareAt(edge1).tile = Some("runwaHEW")
      squareAt(edge2).tile = Some("runwaHSW")


  def placeRunwayVertical(length: Int): Unit = //calledSecond
    require(length > 2)
    val xPos = Random.nextInt(width - bufferSize * 2) + bufferSize
    val yPos = Random.nextInt(height - bufferSize * 2 - length) + bufferSize
    val edge1 = GridPos(xPos, yPos)
    //println(nextNSquares(edge1, East, length - 1).map( _.tile.getOrElse("") ).mkString(","))
    if squareAt(edge1).tile.isDefined
      || !nextNSquares(edge1, South, length + 1).forall( n => n.tile.isEmpty || n.tile.getOrElse("").contains("runwayHH") )
      || !nextNSquares(edge1 + GridPos(0, length - 2), South, 2).forall( _.tile.isEmpty )
      || !nextNSquares(edge1, North, 2).forall( _.tile.isEmpty )
      || !nextNSquares(edge1 + East - South, South, length + 1).forall( n => n.tile.isEmpty || n.tile.getOrElse("").contains("runwayHH") )
      || !nextNSquares(edge1 + West - South, South, length + 1).forall( n => n.tile.isEmpty || n.tile.getOrElse("").contains("runwayHH") ) then
      placeRunwayVertical(length)
    else
      squareAt(edge1).tile = Some("runwaVSS")
      nextNSquares(edge1, South, length - 2).foreach( n => if n.tile.isEmpty then n.tile = Some("runwayVV") else n.tile = Some("runwayVH"))
      val edge2 = GridPos(xPos, yPos + length - 1)
      squareAt(edge2).tile = Some("runwaVES")
      if Random.nextInt(2) == 0 then
        runways.append(Runway(numberOfRunways, edge1, edge2))
      else
        runways.append(Runway(numberOfRunways, edge2, edge1))
        squareAt(edge1).tile = Some("runwaVEN")
        squareAt(edge2).tile = Some("runwaVSN")



enum CompassDir(val xStep: Int, val yStep: Int, val bearing: Int) derives CanEqual:

  case North extends CompassDir( 0,-1, 0)

  case East  extends CompassDir( 1, 0, 90)

  case South extends CompassDir( 0, 1, 180)

  case West  extends CompassDir(-1, 0, 270)



/*

  def collapseOne() =
    val best  = currentGrid.flatten
      .filter(_.tile.isEmpty)
      .minByOption(_.entropy)

    best.foreach( _.collapse() )

case class Neighbor(mine: Int, dir: CompassDir, theirs: Int)

val neighborBoundaries = Vector(
    Neighbor(0, West,  2),
    Neighbor(0, North, 6),
    Neighbor(1, North, 5),
    Neighbor(2, North, 4),
    Neighbor(2, East,  0),
    Neighbor(3, East,  7),
    Neighbor(4, East,  6),
    Neighbor(4, South, 2),
    Neighbor(5, South, 1),
    Neighbor(6, South, 0),
    Neighbor(6, West,  4),
    Neighbor(7, West,  3),
)
*/