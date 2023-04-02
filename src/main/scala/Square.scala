import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.util.Random

class Square(grid: Grid, val loc: GridPos):
  
  var tile = Option.empty[String]
  var boundary       = Buffer.fill[Option[Char]](8)(None)
  var plausibleTiles = ArrayBuffer[String]() ++ AllTiles.allStrings
  
  def entropy = plausibleTiles.size
  
  def fix(placeOnBoundary: Int, value: Char): Unit =
    if boundary(placeOnBoundary).isEmpty then
      boundary(placeOnBoundary) = Some(value)
      plausibleTiles = plausibleTiles.filter( _.charAt(placeOnBoundary) == value )
      propagateChange(placeOnBoundary, value)

  def collapse(): Unit =
    val chosenTile = plausibleTiles(Random.nextInt(plausibleTiles.size))
    tile = Some(chosenTile)
    boundary = chosenTile.toBuffer.map( n => Some(n) )
    boundary.flatten.zipWithIndex.foreach( (n, m) => propagateChange(m, n) )
  
  
  def propagateChange(boundaryIndex: Int, fixedValue: Char): Unit =
    neighborBoundaries.filter( _.mine == boundaryIndex )
      .foreach( n => loc.neighborOption(n.dir, (grid.width, grid.height))
        .foreach( m => grid.squareAt(m).fix(n.theirs, fixedValue)) )