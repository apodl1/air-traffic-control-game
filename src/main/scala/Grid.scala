import CompassDir.{East, North, South, West}

class Grid(val width: Int, val height: Int):

  val currentGrid: Array[Array[Square]] =
    LazyList.iterate(0)(n => n + 1)
      .take(width)
      .toArray
      .map( x =>
        LazyList.iterate(0)(n => n + 1)
          .take(width)
          .toArray
          .map( y => Square(this, GridPos(x, y)) ) )
    
  //TODO collapsing
  
  def squareAt(gridPos: GridPos): Square =
    currentGrid(gridPos.x)(gridPos.y)
    
    
    
    
    

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


enum CompassDir(val xStep: Int, val yStep: Int) derives CanEqual:

  case North extends CompassDir( 0,-1)

  case East  extends CompassDir( 1, 0)

  case South extends CompassDir( 0, 1)

  case West  extends CompassDir(-1, 0)