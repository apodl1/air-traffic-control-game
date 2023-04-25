package airplaneGame

import CompassDir.East

import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.util.Random

//class representing a single square of the grid, starts with empty tile, that may be replaced by grid.generate()
class Square(grid: Grid, val loc: GridPos):

  //corresponding String.png graphic is fetched when rendering. None is renered as ground (black graphic)
  var tile = Option.empty[String]