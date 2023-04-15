package airplaneGame

import CompassDir.East

import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.util.Random

class Square(grid: Grid, val loc: GridPos):

  var tile = Option.empty[String]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  /*
  var boundary       = Buffer.fill[Option[Char]](8)(None)
  var plausibleTiles =
    if loc.x < gridBufferSize || loc.x > grid.width - 1 - gridBufferSize || loc.y < gridBufferSize || loc.y > grid.height - 1 - gridBufferSize then
      ArrayBuffer("00000000")
    else
      ArrayBuffer[String]() ++ AllTiles.allStrings

  
  def entropy = plausibleTiles.size
  
  def fix(placeOnBoundary: Int, value: Char): Unit =
    if boundary(placeOnBoundary).isEmpty then
      boundary(placeOnBoundary) = Some(value)
      plausibleTiles = plausibleTiles.filter( _.charAt(placeOnBoundary) == value )
      propagateChange(placeOnBoundary, value)

  def collapse(): Unit =
    val chosenTile = plausibleTiles(Random.nextInt(plausibleTiles.size))
    tile = Some(chosenTile)
    if chosenTile.count( _ == '1' ) == 1 then
      if otherEndOfRunway(loc).isDefined then
        var edge1 = loc
        var edge2 = otherEndOfRunway(loc).get
        if Random.nextInt(1) == 0 then
          edge1 = otherEndOfRunway(loc).get
          edge2 = loc
        grid.runways.append(Runway(grid.numberOfRunways, edge1, edge2))
      else
        grid.numberOfRunways += 1
      var nextLoc = loc - directionOfRunway(loc)
      while nextLoc.x >= gridBufferSize && nextLoc.x < grid.width - gridBufferSize && nextLoc.y >= gridBufferSize && nextLoc.y < grid.height - gridBufferSize do
        grid.squareAt(nextLoc).excludeRunwaysWithDirection(directionOfRunway(loc))
        nextLoc -= directionOfRunway(loc)
    boundary = chosenTile.toBuffer.map( n => Some(n) )
    boundary.flatten.zipWithIndex.foreach( (n, m) => propagateChange(m, n) )
  
  
  def propagateChange(boundaryIndex: Int, fixedValue: Char): Unit =
    neighborBoundaries.filter( _.mine == boundaryIndex )
      .foreach( n => loc.neighborOption(n.dir, (grid.size))
        .foreach( m => grid.squareAt(m).fix(n.theirs, fixedValue)) )
    if tile.getOrElse("0").contains('1') then
      grid.squareAt(loc + GridPos(directionOfRunway(loc).yStep, directionOfRunway(loc).xStep)).excludeRunways()
      grid.squareAt(loc - GridPos(directionOfRunway(loc).yStep, directionOfRunway(loc).xStep)).excludeRunways()

  def otherEndOfRunway(gridPos: GridPos): Option[GridPos] = //can be called only with edge of runway
    assert(grid.squareAt(gridPos).tile.getOrElse("0").count( _ == '1') == 1)
    val neighborsWithRunway = gridPos.neighbors(grid.size).filter( n => grid.squareAt(n).tile.getOrElse("0").contains('1') )
    if neighborsWithRunway.isEmpty then return None
    val neighborWithRunway = neighborsWithRunway.head
    val direction = loc - neighborWithRunway
    var currentLoc = neighborWithRunway
    while grid.squareAt(currentLoc).tile.getOrElse("0").count(_ == '1') == 2 do
      currentLoc = currentLoc - direction
    if grid.squareAt(currentLoc).tile.getOrElse("0").count(_ == '1') == 0 then
      None
    else
      Some(currentLoc)

  def directionOfRunway(gridPos: GridPos): CompassDir =
    assert(grid.squareAt(gridPos).tile.getOrElse("0").count(_ == '1') > 0)
    grid.squareAt(gridPos).tile.getOrElse("0")
      .toCharArray
      .zipWithIndex
      .filter(_._1 == '1')
      .map(n => neighborBoundaries.filter(_.mine == n._2).head.dir)
      .head

  def excludeRunways() =
    plausibleTiles = plausibleTiles.filter( m => !m.contains('1') )

  def excludeRunwaysWithDirection(direction: CompassDir) =
    if direction == East || direction == CompassDir.West then
      plausibleTiles = plausibleTiles.filter( n => n(3) != '1' && n(7) != '1' )
    else
      plausibleTiles = plausibleTiles.filter( n => n(1) != '1' && n(5) != '1' )

  def includeOnlyRunwaysWithDirection(direction: CompassDir) =
    if direction == East || direction == CompassDir.West then
      fix(3, '1')
      fix(7, '1')
    else
      fix(1, '1')
      fix(5, '1') */