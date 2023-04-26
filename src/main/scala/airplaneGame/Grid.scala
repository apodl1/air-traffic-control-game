package airplaneGame

import CompassDir.{East, North, South, West}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

//a crucial class for representing and generating the game map, gets relevant info as parameters
class Grid(val width: Int, val height: Int, bufferSize: Int, val coordPerTile: Int):

  val size = (width, height)
  val coordSize = (width * coordPerTile, height * coordPerTile)

  val runways = ArrayBuffer.empty[Runway]
  def numberOfRunways = runways.length
  val gates = ArrayBuffer.empty[Gate]
  def numberOfGates = gates.length

  //the grid itself is stored here, elements numbered as (y)(x) counting from top right -corner (as in scala swing GUI). Initialized empty (all squares set as None), filled by generate
  val currentGrid: Vector[Vector[Square]] =
    LazyList.iterate(0)(n => n + 1)
      .take(height)
      .toVector
      .map( y =>
        LazyList.iterate(0)(n => n + 1)
          .take(width)
          .toVector
          .map( x => Square(this, GridPos(x, y)) ) )

  //methods for finding square-objects
  def squareAt(gridPos: GridPos): Square =
    currentGrid(gridPos.y)(gridPos.x)
  def squareAt(x: Int, y: Int): Square =
    currentGrid(y)(x)

  //helper for map generation, finds next n squares in CompasDir direction from starting GridPos (exclusive)
  def nextNSquares(startPos: GridPos, direction: CompassDir, n: Int): Vector[Square] =
    LazyList.iterate(startPos + direction)( _ + direction).take(n).map( n => squareAt(n) ).toVector

  //generates map based on given parameters, shoul donly be called once
  def generate(runways: Int, terminalSize: (Int, Int), runwayLengths: Vector[Int]): Unit =
    //first place terminal
    placeTerminal(terminalSize._1, terminalSize._2)
    //place at least one long runway
    placeRunwayHorizontal(runwayLengths.max)
    //then place number of runways of lengths passed in runwayLengths
    (1 until runways).foreach(n =>
      if Random.nextInt(2) == 0 then
        placeRunwayHorizontal(runwayLengths(Random.nextInt(runwayLengths.length)))
      else
        val possibleLengths = runwayLengths.filter( _ <= height - bufferSize * 2 )
        placeRunwayVertical(possibleLengths(Random.nextInt(possibleLengths.length))))

  //helper for placing the terminal, called first in generate()
  def placeTerminal(terminalWidth: Int, terminalHeight: Int): Unit =
    require(terminalWidth > 2 && terminalHeight > 2) //minimun size
    //forms random GridPos for terminal top-left corner
    val xPos = Random.nextInt(width - (bufferSize - 2) * 2 - terminalWidth) + bufferSize - 1
    val yPos = Random.nextInt(height - (bufferSize - 2) * 2 - terminalHeight) + bufferSize - 1
    //sets appropriate tile and propagates two edges
    val NW = GridPos(xPos, yPos)
    squareAt(NW).tile = Some("terminNW")
    nextNSquares(NW, East, terminalWidth - 2).foreach( n => { n.tile = Some("terminaN"); gates.append(Gate(numberOfGates, n.loc)) } )
    nextNSquares(NW, South, terminalHeight - 2).foreach( n => n.tile = Some("terminaW") )
    //sets appropriate tile and propagates an edge
    val NE = GridPos(xPos + terminalWidth - 1, yPos)
    squareAt(NE).tile = Some("terminNE")
    nextNSquares(NE, South, terminalHeight - 2).foreach( n => n.tile = Some("terminaE") )
    //sets appropriate tile and propagates an edge
    val SW = GridPos(xPos, yPos + terminalHeight - 1)
    squareAt(SW).tile = Some("terminSW")
    nextNSquares(SW, East, terminalWidth - 2).foreach( n => n.tile = Some("terminaS") )
    //sets appropriate tile
    val SE = GridPos(xPos + terminalWidth - 1, yPos + terminalHeight - 1)
    squareAt(SE).tile = Some("terminSE")

  //helper for placing a horizontal runway, called second in generate()
  def placeRunwayHorizontal(length: Int): Unit = //calledSecond
    require(length > 2) //minimum size
    //forms random GridPos for one edge of runway
    val xPos = Random.nextInt(width - bufferSize * 2 - length) + bufferSize
    val yPos = Random.nextInt(height - bufferSize * 2) + bufferSize
    val edge1 = GridPos(xPos, yPos)
    //if position invalid, call function again
    if squareAt(edge1).tile.isDefined
      || !nextNSquares(edge1, East, length + 1).forall( _.tile.isEmpty )
      || !nextNSquares(edge1, West, 2).forall( _.tile.isEmpty )
      || !nextNSquares(edge1 + North - East, East, length + 1).forall( _.tile.isEmpty )
      || !nextNSquares(edge1 + South - East, East, length + 1).forall( _.tile.isEmpty ) then
      placeRunwayHorizontal(length)
    else
      //if position valid, set appropriate tiles
      squareAt(edge1).tile = Some("runwaHSE")
      squareAt(edge1 + North).tile = Some("waitPlaceSH")
      nextNSquares(edge1, East, length - 2).foreach( _.tile = Some("runwayHH") )
      val edge2 = GridPos(xPos + length - 1, yPos)
      squareAt(edge2).tile = Some("runwaHEE")
      squareAt(edge2 + North).tile = Some("waitPlaceEH")
      //randomize true direction of runway and append an object to arrayBuffer in grid
      if Random.nextInt(2) == 0 then
        runways.append(Runway(numberOfRunways, edge1, edge2))
      else
        runways.append(Runway(numberOfRunways, edge2, edge1))
        //reset tiles if nessessary
        squareAt(edge1).tile = Some("runwaHEW")
        squareAt(edge2).tile = Some("runwaHSW")

  //helper for placing a vertical runway, called third in generate(), follows same pattern as horizontal
  def placeRunwayVertical(length: Int): Unit = //calledSecond
    require(length > 2) //minimum size
    //forms random GridPos for one edge of runway
    val xPos = Random.nextInt(width - bufferSize * 2) + bufferSize
    val yPos = Random.nextInt(height - bufferSize * 2 - length) + bufferSize
    val edge1 = GridPos(xPos, yPos)
    //if position invalid, call function again
    if squareAt(edge1).tile.isDefined
      || !nextNSquares(edge1, South, length + 1).forall( n => n.tile.isEmpty || n.tile.getOrElse("").contains("runwayHH") )
      || !nextNSquares(edge1 + GridPos(0, length - 2), South, 2).forall( _.tile.isEmpty )
      || !nextNSquares(edge1, North, 2).forall( _.tile.isEmpty )
      || !nextNSquares(edge1 + East - South, South, length + 1).forall( n => n.tile.isEmpty || n.tile.getOrElse("").contains("runwayHH") )
      || !nextNSquares(edge1 + West - South, South, length + 1).forall( n => n.tile.isEmpty || n.tile.getOrElse("").contains("runwayHH") ) then
      placeRunwayVertical(length)
    else
      //if position valid, set appropriate tiles
      squareAt(edge1).tile = Some("runwaVSS")
      squareAt(edge1 + East).tile = Some("waitPlaceSV")
      nextNSquares(edge1, South, length - 2).foreach( n => if n.tile.isEmpty then n.tile = Some("runwayVV") else n.tile = Some("runwayVH"))
      val edge2 = GridPos(xPos, yPos + length - 1)
      squareAt(edge2).tile = Some("runwaVES")
      squareAt(edge2 + East).tile = Some("waitPlaceEV")
      //randomize true direction of runway and append an object to arrayBuffer in grid
      if Random.nextInt(2) == 0 then
        runways.append(Runway(numberOfRunways, edge1, edge2))
      else
        runways.append(Runway(numberOfRunways, edge2, edge1))
        //reset tiles if nessessary
        squareAt(edge1).tile = Some("runwaVEN")
        squareAt(edge2).tile = Some("runwaVSN")


//enum for repreenting directions on the map, each direction represents one GridPos step. Also contains the bearing of the direction (angle from North)
enum CompassDir(val xStep: Int, val yStep: Int, val bearing: Int) derives CanEqual:

  case North extends CompassDir( 0,-1, 0)

  case East  extends CompassDir( 1, 0, 90)

  case South extends CompassDir( 0, 1, 180)

  case West  extends CompassDir(-1, 0, 270)